package com.vf.wql2pig


/**
 * User: valeryf
 * Date: 12/18/12 12:38 AM
 */

trait WqlConstants extends util.parsing.combinator.RegexParsers {
  val boolean: Parser[BooleanExpr] = ("true" | "false") ^^ {
    s => BooleanExpr(s.toLowerCase.toBoolean)
  }
  val integer: Parser[IntExpr] = """-?\d+""".r ^^ {
    s => IntExpr(s.toInt)
  }

  def unquote(s: String): String = {
    if (s.startsWith("'") && s.endsWith("'"))
      s.substring(1, s.size - 1)
    else s
  }

  val string: Parser[StringExpr] = ("""'""" + """\w*""" + """'""").r ^^ {
    s => StringExpr(unquote(s))
  }
  val ident: Parser[VarExpr] = ("""[a-zA-Z_=*]\w*""".r | failure("couldn't parse identifier")) ^^ {
    s => VarExpr(s)
  }
  val const: Parser[LiteralExpr] = (boolean | integer | string | failure("couldn't parse constant"))
}

trait WqlStatements extends WqlConstants {
  // order users by evid desc
  val direction: Parser[OrderExpr] = ("asc" | "desc") ^^ {
    case "asc" => OrderExpr("asc")
    case "desc" => OrderExpr("desc")
  }

  def combine(column: VarExpr, direction: OrderExpr, s: Any): List[(VarExpr, OrderExpr)] = {
    List((column, direction))
  }

  val order: Parser[AbstractOrder] = ("order" ~ ident ~ "by" | "order" ~ "by") ~ ident ~ direction ~ (("," ~ ident ~ direction) *) ^^ {
    case "order" ~ "by" ~ column ~ dir ~ x =>
      SelectOrderExpr(combine(column, dir, x))
    case "order" ~ (relation: VarExpr) ~ "by" ~ column ~ dir ~ x =>
      FullOrderExpr(relation, combine(column, dir, x))
  }

  val tableStatement: Parser[Expr] = order | select

  val assign: Parser[AssignExpr] = ident ~ "=" ~ tableStatement ^^ {
    case relation ~ "=" ~ expr => AssignExpr(relation, expr)
  }

  val oper: Parser[OperExpr] = (const ~ ident ~ ident | ident ~ ident ~ const | "(" ~ oper ~ ")") ^^ {
    case (left: Expr) ~ VarExpr(operator) ~ (right: Expr) => OperExpr(operator, left, right)
    case "(" ~ (operand: OperExpr) ~ ")" => operand
  }
  val and: Parser[AndExpr] = (oper ~ "and" ~ condition) ^^ {
    case cond1 ~ "and" ~ cond2 => AndExpr(cond1, cond2)
  }
  val or: Parser[OrExpr] = (oper ~ "or" ~ condition) ^^ {
    case cond1 ~ "or" ~ cond2 => OrExpr(cond1, cond2)
  }
  val condition: Parser[ConditionExpr] = ((and | or | oper) | ("(" ~ condition ~ ")")) ^^ {
    case cond: ConditionExpr => cond
    case "(" ~ (cond: ConditionExpr) ~ ")" => cond
  }

  val where: Parser[AbstractWhereExpr] = "where" ~ condition ^^ {
    case "where" ~ cond => WhereExpr(cond)
  }

  val select: Parser[SelectExpr] = "select" ~ ident ~ "from" ~ ident ~ opt(where) ^^ {
    case "select" ~ VarExpr(column) ~ "from" ~ relation ~ whereStmt =>
      SelectExpr(ColumnsExpr(List(column)), relation, whereStmt.getOrElse(EmptyWhereExpr()), EmptyOrder())
  }
}