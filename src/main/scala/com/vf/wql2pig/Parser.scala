package com.vf.wql2pig

/**
 * User: valeryf
 * Date: 12/17/12 1:25 AM
 */
object Parser {

  def isDelimiter(token: Token): Boolean = {
    token match {
      case _: EqSign => true
      case _: SemiColon => true
      case StringToken("from") => true
      case StringToken("where") => true
      case StringToken("order") => true
      case StringToken("join") => true
      case _ => false
    }
  }

  def extractExprsAndRest(tokens: List[Token]): (List[Expr], List[Token]) = {
    tokens match {
      case Nil => (Nil, Nil)
      case _ => extractExprAndRest(tokens) match {
        case (value: Expr, token :: rest) if isDelimiter(token) => (List(value), token :: rest)
        case (value: Expr, rest) => extractExprsAndRest(rest) match {
          case (values: List[Expr], rest2) => (value :: values, rest2)
          case (_, rest3) => throw new Exception("Could not extract exprs and rest from " + tokens)
        }
      }
    }
  }

  def createColumns(exprs: List[Expr]): AbstractColumnsExpr = {
    exprs match {
      case StringExpr("*") :: Nil => AllColumnsExpr()
      case _ => {
        val mapped = exprs map {
          case StringExpr(x) => x
          case _ => throw new Exception("Error : createColumns : expected column names or *, got " + exprs)
        }
        ColumnsExpr(mapped)
      }
    }
  }

  def extractExprAndRest(tokens: List[Token]): (Expr, List[Token]) = {
    tokens match {
      case IntToken(x) :: rest => (IntExpr(x), rest)
      case StringToken(x) :: EqSign() :: rest => {
        extractExprAndRest(rest) match {
          case (select: SelectExpr, rest2) => (AssignExpr(VarExpr(x), select), rest2)
          case _ => throw new Exception("Error : could not extract the assign value of variable " + x + " from tokens " + rest)
        }
      }

      case StringToken("select") :: rest => {
        extractExprsAndRest(rest) match {
          case (exprs, StringToken("from") :: StringToken(tableName) :: SemiColon() :: rest2) => (SelectExpr(createColumns(exprs), VarExpr(tableName), EmptyWhereExpr(), EmptyOrder()), rest2)
          case _ => throw new Exception("Error : could not extract select statement from " + rest)
        }
      }


      case StringToken(x) :: rest => (StringExpr(x), rest)
      case _ => throw new Exception("Could not extract expr and rest from " + tokens + ", probably due to missing right paren")
    }
  }

  def parse(tokens: List[Token]): Expr = {
    extractExprAndRest(tokens) match {
      case (expr, Nil) => expr
    }
  }

  def parse(line: String): Expr = {
    val tokens = Scanner.scan(line)
    parse(tokens)
  }

}
