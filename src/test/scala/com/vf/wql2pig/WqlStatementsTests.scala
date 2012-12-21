package com.vf.wql2pig

import util.parsing.input.CharSequenceReader
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec

/**
 * User: valeryf
 * Date: 12/18/12 1:14 AM
 */
class WqlStatementsTests extends WqlStatements with ShouldMatchers with FlatSpec {

  private def parsing[T](s: String)(implicit p: Parser[T]): T = {
    //wrap the parser in the phrase parse to make sure all input is consumed
    val phraseParser = phrase(p)
    //we need to wrap the string in a reader so our parser can digest it
    val input = new CharSequenceReader(s)
    phraseParser(input) match {
      case Success(t, _) => t
      case NoSuccess(msg, _) => throw new IllegalArgumentException(
        "Could not parse '" + s + "' due to [" + msg + "]")
    }
  }

  private def assertFail[T](input: String)(implicit p: Parser[T]) {
    evaluating(parsing(input)(p)) should produce[IllegalArgumentException]
  }

  "The WqlConstants" should "parse simpleorder statement" in {
    implicit val parserToTest = this.simpleOrder
    parsing("asc") should equal(OrderExpr("asc"))
    parsing("desc") should equal(OrderExpr("desc"))
  }

  they should "parse full order statement" in {
    implicit val parserToTest = this.order
    parsing("order users by evid asc") should equal(FullOrderExpr(VarExpr("users"), List((VarExpr("evid"), OrderExpr("asc")))))
  }

  they should "parse asign statement" in {
    implicit val parserToTest = this.assign
    parsing("users = order users by evid asc") should equal(AssignExpr(VarExpr("users"), FullOrderExpr(VarExpr("users"), List((VarExpr("evid"), OrderExpr("asc"))))))
  }

  they should "parse select statement" in {
    implicit val parserToTest = this.select
    parsing("select * from users") should equal(SelectExpr(ColumnsExpr(List("*")), VarExpr("users"), EmptyWhereExpr(), EmptyOrder()))
    parsing("select evid from users") should equal(SelectExpr(ColumnsExpr(List("evid")), VarExpr("users"), EmptyWhereExpr(), EmptyOrder()))
  }

  they should "parse simple condition expressions" in {
    implicit val parserToTest = this.oper
    parsing("x = 3") should equal(OperExpr("=", VarExpr("x"), IntExpr(3)))
    parsing("(x = 3)") should equal(OperExpr("=", VarExpr("x"), IntExpr(3)))
  }

  they should "parse and expressions" in {
    implicit val parserToTest = this.and
    parsing("x = 2 and y = 3") should equal(AndExpr(OperExpr("=", VarExpr("x"), IntExpr(2)), OperExpr("=", VarExpr("y"), IntExpr(3))))
    parsing("(x = 2) and (y = 3)") should equal(AndExpr(OperExpr("=", VarExpr("x"), IntExpr(2)), OperExpr("=", VarExpr("y"), IntExpr(3))))
  }

  they should "parse or expressions" in {
    implicit val parserToTest = this.or
    parsing("x = 2 or y = 3") should equal(OrExpr(OperExpr("=", VarExpr("x"), IntExpr(2)), OperExpr("=", VarExpr("y"), IntExpr(3))))
    parsing("(x = 2) or (y = 3)") should equal(OrExpr(OperExpr("=", VarExpr("x"), IntExpr(2)), OperExpr("=", VarExpr("y"), IntExpr(3))))
  }

  they should "parse complex conditional expressions" in {
    implicit val parserToTest = this.condition
    parsing("x = 2 and (x = 4 or y = 5)") should equal(AndExpr(OperExpr("=", VarExpr("x"), IntExpr(2)), OrExpr(OperExpr("=", VarExpr("x"), IntExpr(4)), OperExpr("=", VarExpr("y"), IntExpr(5)))))
    parsing("x = 2 and y = 3 and z = 6") should equal(AndExpr(OperExpr("=", VarExpr("x"), IntExpr(2)), AndExpr(OperExpr("=", VarExpr("y"), IntExpr(3)), OperExpr("=", VarExpr("z"), IntExpr(6)))))
  }
}
