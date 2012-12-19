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
  val ident: Parser[VarExpr] = """[a-zA-Z_]\w*""".r ^^ {
    s => VarExpr(s)
  }
}

trait WqlStatements extends WqlConstants {
  // order users by evid desc
  val simpleOrder: Parser[SimpleOrderExpr] = ("asc" | "desc") ^^ {
    case "asc" => AscOrder()
    case "desc" => DescOrder()
  }

  val order: Parser[FullOrderExpr] = "order" ~ ident ~ "by" ~ ident ~ simpleOrder ^^ {
    case "order" ~ relation ~ "by" ~ column ~ someOrder => FullOrderExpr(relation, List((column, someOrder)))
  }

  val tableStatement: Parser[Expr] = order | select

  val assign: Parser[AssignExpr] = ident ~ "=" ~ tableStatement ^^ {
    case relation ~ "=" ~ expr => AssignExpr(relation, expr)
  }

  val oper : Parser[OperExpr] = (((boolean | integer | string) ~ ident ~ ident) | (ident ~ ident ~ (boolean | integer | string))) ^^ {
    case x ~ VarExpr(operator) ~ field => OperExpr(operator, x, field)
  }
  val and : Parser[AndExpr] = (condition ~ "and" ~ condition) ^^ {case cond1 ~ "and" ~ cond2 => AndExpr(cond1, cond2)}
  val or : Parser[OrExpr] = (condition ~ "or" ~ condition) ^^ {case cond1 ~ "or" ~ cond2 => OrExpr(cond1, cond2)}
  val condition : Parser[ConditionExpr] = (oper | and | or);

  val where : Parser[AbstractWhereExpr] = "where" ~ condition ^^ => {
    case "where" ~ cond => WhereExpr(cond)
  }

  val select: Parser[SelectExpr] = "select" ~ ("*" | ident) ~ "from" ~ ident ^^ {
    case "select" ~ "*" ~ "from" ~ relation => SelectExpr(AllColumnsExpr(), relation, EmptyWhereExpr(), EmptyOrder())
    case "select" ~ VarExpr(column) ~ "from" ~ relation => SelectExpr(ColumnsExpr(List(column)), relation, EmptyWhereExpr(), EmptyOrder())
  }
}