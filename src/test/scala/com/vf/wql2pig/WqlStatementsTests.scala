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
    implicit val parserToTest = this.direction
    parsing("asc") should equal(OrderExpr("asc"))
    parsing("desc") should equal(OrderExpr("desc"))
    assertFail("ascc")
  }

  they should "parse full order statement" in {
    implicit val parserToTest = this.order

    val evid_asc = (VarExpr("evid"), OrderExpr("asc"))
    val users = VarExpr("users")

    parsing("order users by evid asc") should equal(FullOrderExpr(users, List(evid_asc)))

    parsing("order by evid asc") should equal(SelectOrderExpr(List(evid_asc)))

    parsing("order by evid asc, date_created desc, src asc") should equal(SelectOrderExpr(
      List(evid_asc, (VarExpr("date_created"), OrderExpr("desc")), (VarExpr("src"), OrderExpr("asc")))))

    parsing("order users by evid asc, date_created desc, src asc") should equal(FullOrderExpr(users,
      List(evid_asc, (VarExpr("date_created"), OrderExpr("desc")), (VarExpr("src"), OrderExpr("asc")))))
  }

  they should "parse asign statement" in {
    implicit val parserToTest = this.assign
    parsing("users = order users by evid asc") should equal(AssignExpr(VarExpr("users"), FullOrderExpr(VarExpr("users"), List((VarExpr("evid"), OrderExpr("asc"))))))
  }

  they should "parse select statement" in {
    implicit val parserToTest = this.select
    parsing("select * from users") should equal(SelectExpr(ColumnsExpr(List("*")), VarExpr("users"), EmptyWhereExpr(), EmptyOrder()))
    parsing("select evid from users") should equal(SelectExpr(ColumnsExpr(List("evid")), VarExpr("users"), EmptyWhereExpr(), EmptyOrder()))
    parsing("select evid from users where src = 3") should equal(SelectExpr(ColumnsExpr(List("evid")), VarExpr("users"), WhereExpr(OperExpr("=", VarExpr("src"), IntExpr(3))), EmptyOrder()))
    parsing("select evid from users where src = 3 order by evid desc") should equal(SelectExpr(ColumnsExpr(List("evid")), VarExpr("users"), WhereExpr(OperExpr("=", VarExpr("src"), IntExpr(3))), SelectOrderExpr(List((VarExpr("evid"), OrderExpr("desc"))))))
  }

  they should "parse simple condition expressions" in {
    implicit val parserToTest = this.oper
    parsing("x = 3") should equal(OperExpr("=", VarExpr("x"), IntExpr(3)))
    parsing("(x = 3)") should equal(OperExpr("=", VarExpr("x"), IntExpr(3)))

    assertFail("x = 3 x")
    assertFail("(x = 3")
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

  they should "parse where expressions" in {
    implicit val parserToTest = this.where
    parsing("where x = 2 or y = 3") should equal(WhereExpr(OrExpr(OperExpr("=", VarExpr("x"), IntExpr(2)), OperExpr("=", VarExpr("y"), IntExpr(3)))))
    parsing("where (x = 2) or (y = 3)") should equal(WhereExpr(OrExpr(OperExpr("=", VarExpr("x"), IntExpr(2)), OperExpr("=", VarExpr("y"), IntExpr(3)))))
  }

  they should "parse join expressions" in {
    implicit val parserToTest = this.join
    parsing("join users by uuid, events by uuid, rest by uuid") should equal(JoinExpr(
      List((VarExpr("users"), VarExpr("uuid")), (VarExpr("events"), VarExpr("uuid")), (VarExpr("rest"), VarExpr("uuid")))))
  }
}
