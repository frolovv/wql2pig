package com.vf.wql.parser

import util.parsing.input.CharSequenceReader
import com.vf.wql.definitions._
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat


/**
 * User: valeryf
 * Date: 12/18/12 12:38 AM
 */

trait WqlConstants extends util.parsing.combinator.RegexParsers {
  val boolean: Parser[WqlBoolean] = ("true" | "false") ^^ {
    s => WqlBoolean(s.toLowerCase.toBoolean)
  }
  val integer: Parser[WqlInt] = """-?\d+""".r ^^ {
    s => WqlInt(s.toInt)
  }

  def unquote(s: String): String = {
    if (s.startsWith("'") && s.endsWith("'"))
      s.substring(1, s.size - 1)
    else s
  }

  val string: Parser[WqlString] = ("""'""" + """(\w|-)*""" + """'""").r ^^ {
    s => WqlString(unquote(s))
  }
  val wqlnull: Parser[WqlNull] = "null".r ^^ (_ => WqlNull())

  val ident: Parser[WqlVar] = ("""[a-zA-Z_=*](\w|=)*""".r | failure("couldn't parse identifier")) ^^ {
    s => WqlVar(s)
  }
  val const: Parser[LiteralWqlExpr] = (boolean | integer | string | wqlnull | failure("couldn't parse constant"))

  val arg: Parser[WqlExpr] = const | ident
}

trait WqlStatements extends WqlConstants {
  // order users by evid desc
  val direction: Parser[WqlDirection] = ("asc" | "desc") ^^ {
    case "asc" => WqlDirection("asc")
    case "desc" => WqlDirection("desc")
  }

  def combine(column: WqlVar, direction: WqlDirection, s: List[WqlVar ~ WqlDirection]): List[(WqlVar, WqlDirection)] = {
    (column, direction) :: (s map (x => (x._1, x._2)))
  }

  val order: Parser[WqlAbstractOrder] = ("order" ~ ident ~ "by" | "order" ~ "by") ~ ident ~ direction ~ (("," ~> ident ~ direction) *) ^^ {
    case "order" ~ "by" ~ column ~ dir ~ x =>
      WqlSelectOrder(combine(column, dir, x))
    case "order" ~ (relation: WqlVar) ~ "by" ~ column ~ dir ~ x =>
      WqlFullOrder(relation, combine(column, dir, x))
  }

  val join: Parser[WqlJoin] = ("join " ~> ident) ~ ("by" ~> ident) ~ (("," ~> ident ~ ("by" ~> ident)) *) ^^ {
    case table ~ column ~ rest => {
      val mapped = rest map (x => (x._1, x._2))
      WqlJoin((table, column) :: mapped)
    }
  }

  val func: Parser[WqlFunc] = ident ~ "(" ~ arg ~ opt(("," ~> arg) *) ~ ")" ^^ {
    case name ~ "(" ~ first ~ rest ~ ")" => WqlFunc(name.name, first :: rest.getOrElse(Nil))
  }

  val tableStatement: Parser[WqlExpr] = order | select | join | filter

  val assign: Parser[WqlAssign] = ident ~ "=" ~ tableStatement ^^ {
    case relation ~ "=" ~ expr => WqlAssign(relation, expr)
  }

  val oper: Parser[WqlOper] = (const ~ ident ~ ident | ident ~ ident ~ const | "(" ~ oper ~ ")") ^^ {
    case (left: WqlExpr) ~ WqlVar(operator) ~ (right: WqlExpr) => WqlOper(operator, left, right)
    case "(" ~ (operand: WqlOper) ~ ")" => operand
  }

  val opernull: Parser[WqlOperNull] = ident ~ "is" ~ opt("not") ~ wqlnull ^^ {
    case left ~ "is" ~ not ~ nullvalue => WqlOperNull(left, not)
  }

  val and: Parser[WqlAnd] = (oper ~ "and" ~ condition) ^^ {
    case cond1 ~ "and" ~ cond2 => WqlAnd(cond1, cond2)
  }

  val or: Parser[WqlOr] = (oper ~ "or" ~ condition) ^^ {
    case cond1 ~ "or" ~ cond2 => WqlOr(cond1, cond2)
  }

  val condition: Parser[WqlCondition] = ((and | or | opernull | oper) | ("(" ~> condition <~ ")")) ^^ {
    case cond: WqlCondition => cond
  }

  val where: Parser[WqlWhere] = "where" ~> condition ^^ (cond => WqlWhere(cond))

  val wherekey: Parser[WqlAbstractWhere] = (((("wherekey" ~ "src" ~ "=") ~> integer) ~ (("and" ~ "date_created" ~ "between" ~ "(") ~> string) ~ ("," ~> string <~ ")")) | (("wherekey" ~ "src" ~ "=") ~> integer)) ^^ {
    case (src: WqlInt) ~ (start: WqlString) ~ (end: WqlString) => WqlWhereKey(src.value.toString, start.str, end.str)
    case (src: WqlInt) => {
      val fmt = DateTimeFormat.forPattern("yyyy-MM-dd")
      val dt = new DateTime()
      WqlWhereKey(src.value.toString, fmt.print(dt.minusDays(1)), fmt.print(dt))
    }
  }

  val group: Parser[WqlGroup] = (("group" ~ "by") ~> ident) ~ (("," ~> ident) *) ^^ {
    case first ~ rest => WqlGroup(first :: rest)
  }

  val filter: Parser[WqlFilter] = ("filter" ~> ident) ~ ("by" ~> condition) ^^ {
    case relation ~ conditions => WqlFilter(relation, conditions)
  }

  val select: Parser[WqlAbstractSelect] = "select" ~ ident ~ opt(("," ~> ident) *) ~ "from" ~ ident ~ opt(wherekey) ~ opt(where) ~ opt(group) ~ opt(order) ^^ {
    case "select" ~ WqlVar(column) ~ columns ~ "from" ~ relation ~ None ~ whereStmt ~ groupStmt ~ orderStmt => {
      var result: WqlAbstractSelect = WqlSelect(column :: columns.getOrElse(Nil).map(_.name), relation)

      if (groupStmt.isDefined)
        result = WqlSelectWithGroup(result, groupStmt.get)
      if (whereStmt.isDefined)
        result = WqlSelectWithWhere(result, whereStmt.get)
      if (orderStmt.isDefined)
        result = WqlSelectWithOrder(result, orderStmt.get.asInstanceOf[WqlSelectOrder])
      result
    }
    case "select" ~ WqlVar(column) ~ columns ~ "from" ~ relation ~ Some(whereKey: WqlWhereKey) ~ whereStmt ~ None ~ orderStmt => {
      var result: WqlAbstractSelect = WqlSelect(column :: columns.getOrElse(Nil).map(_.name), relation)
      result = WqlSelectWithTBL(result, whereKey)
      if (whereStmt.isDefined)
        result = WqlSelectWithWhere(result, whereStmt.get)
      if (orderStmt.isDefined)
        result = WqlSelectWithOrder(result, orderStmt.get.asInstanceOf[WqlSelectOrder])
      result
    }
  }
}

trait WqlParser extends WqlStatements {
  val wqlParser: Parser[List[WqlExpr]] = assign.*

  def parse(line: String): List[WqlExpr] = {
    val input = new CharSequenceReader(line)
    wqlParser(input) match {
      case Success(result, _) => result
      case _ => throw new IllegalArgumentException("Could not parse " + line)
    }
  }
}