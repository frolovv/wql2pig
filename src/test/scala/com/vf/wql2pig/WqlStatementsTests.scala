package com.vf.wql2pig

import definitions.EmptyOrder
import util.parsing.input.CharSequenceReader
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import com.vf.wql2pig.definitions._

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
    parsing("asc") should equal(OrderWqlExpr("asc"))
    parsing("desc") should equal(OrderWqlExpr("desc"))
    assertFail("ascc")
  }

  they should "parse full order statement" in {
    implicit val parserToTest = this.order

    val evid_asc = (VarWqlExpr("evid"), OrderWqlExpr("asc"))
    val users = VarWqlExpr("users")

    parsing("order users by evid asc") should equal(FullOrderWqlExpr(users, List(evid_asc)))

    parsing("order by evid asc") should equal(SelectOrderWqlExpr(List(evid_asc)))

    parsing("order by evid asc, date_created desc, src asc") should equal(SelectOrderWqlExpr(
      List(evid_asc, (VarWqlExpr("date_created"), OrderWqlExpr("desc")), (VarWqlExpr("src"), OrderWqlExpr("asc")))))

    parsing("order users by evid asc, date_created desc, src asc") should equal(FullOrderWqlExpr(users,
      List(evid_asc, (VarWqlExpr("date_created"), OrderWqlExpr("desc")), (VarWqlExpr("src"), OrderWqlExpr("asc")))))
  }

  they should "parse asign statement" in {
    implicit val parserToTest = this.assign
    parsing("users = order users by evid asc") should equal(AssignWqlExpr(VarWqlExpr("users"), FullOrderWqlExpr(VarWqlExpr("users"), List((VarWqlExpr("evid"), OrderWqlExpr("asc"))))))
  }

  they should "parse select statement" in {
    implicit val parserToTest = this.select
    parsing("select * from users") should equal(SelectWqlExpr(ColumnsWqlExpr(List("*")), VarWqlExpr("users"), EmptyWhereWqlExpr(), EmptyOrder()))
    parsing("select evid from users") should equal(SelectWqlExpr(ColumnsWqlExpr(List("evid")), VarWqlExpr("users"), EmptyWhereWqlExpr(), EmptyOrder()))
    parsing("select evid from users where src = 3") should equal(SelectWqlExpr(ColumnsWqlExpr(List("evid")), VarWqlExpr("users"), WhereWqlExpr(OperWqlExpr("=", VarWqlExpr("src"), IntWqlExpr(3))), EmptyOrder()))
    parsing("select evid from users where src = 3 order by evid desc") should equal(SelectWqlExpr(ColumnsWqlExpr(List("evid")), VarWqlExpr("users"), WhereWqlExpr(OperWqlExpr("=", VarWqlExpr("src"), IntWqlExpr(3))), SelectOrderWqlExpr(List((VarWqlExpr("evid"), OrderWqlExpr("desc"))))))
  }

  they should "parse simple condition expressions" in {
    implicit val parserToTest = this.oper
    parsing("x = 3") should equal(OperWqlExpr("=", VarWqlExpr("x"), IntWqlExpr(3)))
    parsing("(x = 3)") should equal(OperWqlExpr("=", VarWqlExpr("x"), IntWqlExpr(3)))

    assertFail("x = 3 x")
    assertFail("(x = 3")
  }

  they should "parse and expressions" in {
    implicit val parserToTest = this.and
    parsing("x = 2 and y = 3") should equal(AndWqlExpr(OperWqlExpr("=", VarWqlExpr("x"), IntWqlExpr(2)), OperWqlExpr("=", VarWqlExpr("y"), IntWqlExpr(3))))
    parsing("(x = 2) and (y = 3)") should equal(AndWqlExpr(OperWqlExpr("=", VarWqlExpr("x"), IntWqlExpr(2)), OperWqlExpr("=", VarWqlExpr("y"), IntWqlExpr(3))))
  }

  they should "parse or expressions" in {
    implicit val parserToTest = this.or
    parsing("x = 2 or y = 3") should equal(OrWqlExpr(OperWqlExpr("=", VarWqlExpr("x"), IntWqlExpr(2)), OperWqlExpr("=", VarWqlExpr("y"), IntWqlExpr(3))))
    parsing("(x = 2) or (y = 3)") should equal(OrWqlExpr(OperWqlExpr("=", VarWqlExpr("x"), IntWqlExpr(2)), OperWqlExpr("=", VarWqlExpr("y"), IntWqlExpr(3))))
  }

  they should "parse complex conditional expressions" in {
    implicit val parserToTest = this.condition
    parsing("x = 2 and (x = 4 or y = 5)") should equal(AndWqlExpr(OperWqlExpr("=", VarWqlExpr("x"), IntWqlExpr(2)), OrWqlExpr(OperWqlExpr("=", VarWqlExpr("x"), IntWqlExpr(4)), OperWqlExpr("=", VarWqlExpr("y"), IntWqlExpr(5)))))
    parsing("x = 2 and y = 3 and z = 6") should equal(AndWqlExpr(OperWqlExpr("=", VarWqlExpr("x"), IntWqlExpr(2)), AndWqlExpr(OperWqlExpr("=", VarWqlExpr("y"), IntWqlExpr(3)), OperWqlExpr("=", VarWqlExpr("z"), IntWqlExpr(6)))))
  }

  they should "parse where expressions" in {
    implicit val parserToTest = this.where
    parsing("where x = 2 or y = 3") should equal(WhereWqlExpr(OrWqlExpr(OperWqlExpr("=", VarWqlExpr("x"), IntWqlExpr(2)), OperWqlExpr("=", VarWqlExpr("y"), IntWqlExpr(3)))))
    parsing("where (x = 2) or (y = 3)") should equal(WhereWqlExpr(OrWqlExpr(OperWqlExpr("=", VarWqlExpr("x"), IntWqlExpr(2)), OperWqlExpr("=", VarWqlExpr("y"), IntWqlExpr(3)))))
  }

  they should "parse join expressions" in {
    implicit val parserToTest = this.join
    parsing("join users by uuid, events by uuid, rest by uuid") should equal(JoinWqlExpr(
      List((VarWqlExpr("users"), VarWqlExpr("uuid")), (VarWqlExpr("events"), VarWqlExpr("uuid")), (VarWqlExpr("rest"), VarWqlExpr("uuid")))))
  }

  they should "parse filter expressions" in {
    implicit val parserToTest = this.filter
    parsing("filter users by (evid == 104)") should equal(FilterWqlExpr(VarWqlExpr("users"), OperWqlExpr("==", VarWqlExpr("evid"), IntWqlExpr(104))))
    parsing("filter users by evid == 104") should equal(FilterWqlExpr(VarWqlExpr("users"), OperWqlExpr("==", VarWqlExpr("evid"), IntWqlExpr(104))))
    parsing("filter users by (evid == 104) and src == 3") should equal(FilterWqlExpr(VarWqlExpr("users"), AndWqlExpr(OperWqlExpr("==", VarWqlExpr("evid"), IntWqlExpr(104)), OperWqlExpr("==", VarWqlExpr("src"), IntWqlExpr(3)))))
  }
}
