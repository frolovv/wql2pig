package com.vf.wql.parser

import util.parsing.input.CharSequenceReader
import com.vf.wql.definitions._


/**
 * User: valeryf
 * Date: 12/18/12 12:38 AM
 */

trait WqlConstants extends util.parsing.combinator.RegexParsers {
  val boolean: Parser[BooleanWqlExpr] = ("true" | "false") ^^ {
    s => BooleanWqlExpr(s.toLowerCase.toBoolean)
  }
  val integer: Parser[IntWqlExpr] = """-?\d+""".r ^^ {
    s => IntWqlExpr(s.toInt)
  }

  def unquote(s: String): String = {
    if (s.startsWith("'") && s.endsWith("'"))
      s.substring(1, s.size - 1)
    else s
  }

  val string: Parser[StringWqlExpr] = ("""'""" + """\w*""" + """'""").r ^^ {
    s => StringWqlExpr(unquote(s))
  }
  val ident: Parser[VarWqlExpr] = ("""[a-zA-Z_=*](\w|=)*""".r | failure("couldn't parse identifier")) ^^ {
    s => VarWqlExpr(s)
  }
  val const: Parser[LiteralWqlExpr] = (boolean | integer | string | failure("couldn't parse constant"))
}

trait WqlStatements extends WqlConstants {
  // order users by evid desc
  val direction: Parser[OrderWqlExpr] = ("asc" | "desc") ^^ {
    case "asc" => OrderWqlExpr("asc")
    case "desc" => OrderWqlExpr("desc")
  }

  def combine(column: VarWqlExpr, direction: OrderWqlExpr, s: List[VarWqlExpr ~ OrderWqlExpr]): List[(VarWqlExpr, OrderWqlExpr)] = {
    (column, direction) :: (s map (x => (x._1, x._2)))
  }

  val order: Parser[AbstractOrder] = ("order" ~ ident ~ "by" | "order" ~ "by") ~ ident ~ direction ~ (("," ~> ident ~ direction) *) ^^ {
    case "order" ~ "by" ~ column ~ dir ~ x =>
      SelectOrderWqlExpr(combine(column, dir, x))
    case "order" ~ (relation: VarWqlExpr) ~ "by" ~ column ~ dir ~ x =>
      FullOrderWqlExpr(relation, combine(column, dir, x))
  }

  val join: Parser[JoinWqlExpr] = ("join " ~> ident) ~ ("by" ~> ident) ~ (("," ~> ident ~ ("by" ~> ident)) *) ^^ {
    case table ~ column ~ rest => {
      val mapped = rest map (x => (x._1, x._2))
      JoinWqlExpr((table, column) :: mapped)
    }
  }

  val tableStatement: Parser[WqlExpr] = order | select | join | filter

  val assign: Parser[AssignWqlExpr] = ident ~ "=" ~ tableStatement ^^ {
    case relation ~ "=" ~ expr => AssignWqlExpr(relation, expr)
  }

  val oper: Parser[OperWqlExpr] = (const ~ ident ~ ident | ident ~ ident ~ const | "(" ~ oper ~ ")") ^^ {
    case (left: WqlExpr) ~ VarWqlExpr(operator) ~ (right: WqlExpr) => OperWqlExpr(operator, left, right)
    case "(" ~ (operand: OperWqlExpr) ~ ")" => operand
  }
  val and: Parser[AndWqlExpr] = (oper ~ "and" ~ condition) ^^ {
    case cond1 ~ "and" ~ cond2 => AndWqlExpr(cond1, cond2)
  }
  val or: Parser[OrWqlExpr] = (oper ~ "or" ~ condition) ^^ {
    case cond1 ~ "or" ~ cond2 => OrWqlExpr(cond1, cond2)
  }
  val condition: Parser[ConditionWqlExpr] = ((and | or | oper) | ("(" ~> condition <~ ")")) ^^ {
    case cond: ConditionWqlExpr => cond
  }

  val where: Parser[AbstractWhereWqlExpr] = "where" ~> condition ^^ (cond => WhereWqlExpr(cond))

  val filter: Parser[FilterWqlExpr] = ("filter" ~> ident) ~ ("by" ~> condition) ^^ {
    case relation ~ conditions => FilterWqlExpr(relation, conditions)
  }

  val select: Parser[SelectWqlExpr] = "select" ~ ident ~ "from" ~ ident ~ opt(where) ~ opt(order) ^^ {
    case "select" ~ VarWqlExpr(column) ~ "from" ~ relation ~ whereStmt ~ orderStmt =>
      SelectWqlExpr(ColumnsWqlExpr(List(column)), relation, whereStmt.getOrElse(EmptyWhereWqlExpr()), orderStmt.getOrElse(EmptyOrder()))
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