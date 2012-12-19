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
    parsing("asc") should equal(AscOrder())
    parsing("desc") should equal(DescOrder())
  }

  they should "parse full order statement" in {
    implicit val parserToTest = this.order
    parsing("order users by evid asc") should equal(FullOrderExpr(VarExpr("users"), List((VarExpr("evid"), AscOrder()))))
  }

  they should "parse asign statement" in {
    implicit val parserToTest = this.assign
    parsing("users = order users by evid asc") should equal(AssignExpr(VarExpr("users"), FullOrderExpr(VarExpr("users"), List((VarExpr("evid"), AscOrder())))))
  }

  they should "parse select statement" in {
    implicit val parserToTest = this.select
    parsing("select * from users") should equal(SelectExpr(AllColumnsExpr(), VarExpr("users"), EmptyWhereExpr(), EmptyOrder()))
    parsing("select evid from users") should equal(SelectExpr(ColumnsExpr(List("evid")), VarExpr("users"), EmptyWhereExpr(), EmptyOrder()))
  }

}
