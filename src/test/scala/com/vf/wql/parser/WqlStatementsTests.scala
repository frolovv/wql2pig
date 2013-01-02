package com.vf.wql.parser

import util.parsing.input.CharSequenceReader
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import com.vf.wql.definitions._

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
    parsing("asc") should equal(WqlDirection("asc"))
    parsing("desc") should equal(WqlDirection("desc"))
    assertFail("ascc")
  }

  they should "parse full order statement" in {
    implicit val parserToTest = this.order

    val evid_asc = (WqlVar("evid"), WqlDirection("asc"))
    val users = WqlVar("users")

    parsing("order users by evid asc") should equal(WqlFullOrder(users, List(evid_asc)))

    parsing("order by evid asc") should equal(WqlSelectOrder(List(evid_asc)))

    parsing("order by evid asc, date_created desc, src asc") should equal(WqlSelectOrder(
      List(evid_asc, (WqlVar("date_created"), WqlDirection("desc")), (WqlVar("src"), WqlDirection("asc")))))

    parsing("order users by evid asc, date_created desc, src asc") should equal(WqlFullOrder(users,
      List(evid_asc, (WqlVar("date_created"), WqlDirection("desc")), (WqlVar("src"), WqlDirection("asc")))))
  }

  they should "parse asign statement" in {
    implicit val parserToTest = this.assign
    parsing("users = order users by evid asc") should equal(WqlAssign(WqlVar("users"), WqlFullOrder(WqlVar("users"), List((WqlVar("evid"), WqlDirection("asc"))))))
  }

  they should "parse select statement" in {
    implicit val parserToTest = this.select
    parsing("select * from users") should equal(WqlSelect(List("*"), WqlVar("users"), WqlEmptyWhere(), WqlEmptyWhere(), WqlEmptyOrder()))
    parsing("select evid from users") should equal(WqlSelect(List("evid"), WqlVar("users"), WqlEmptyWhere(), WqlEmptyWhere(), WqlEmptyOrder()))
    parsing("select evid from users where src = 3") should equal(WqlSelect(List("evid"), WqlVar("users"), WqlEmptyWhere(), WqlWhere(WqlOper("=", WqlVar("src"), WqlInt(3))), WqlEmptyOrder()))
    parsing("select evid from users where src = 3 order by evid desc") should equal(WqlSelect(List("evid"), WqlVar("users"), WqlEmptyWhere(), WqlWhere(WqlOper("=", WqlVar("src"), WqlInt(3))), WqlSelectOrder(List((WqlVar("evid"), WqlDirection("desc"))))))

    parsing("select evid, uuid from users") should equal(WqlSelect(List("evid", "uuid"), WqlVar("users"), WqlEmptyWhere(), WqlEmptyWhere(), WqlEmptyOrder()))

    parsing("select evid from users wherekey src = 3 and date_created between('2012-15-16', '2012-16-18')") should equal(
      WqlSelect(List("evid"), WqlVar("users"), WqlWhereKey("3", "2012-15-16", "2012-16-18"), WqlEmptyWhere(), WqlEmptyOrder())
    )

    parsing("select evid from users wherekey src=3 and date_created between ('2012-15-16','2012-16-18')") should equal(
      WqlSelect(List("evid"), WqlVar("users"), WqlWhereKey("3", "2012-15-16", "2012-16-18"), WqlEmptyWhere(), WqlEmptyOrder())
    )
  }

  they should "parse simple condition expressions" in {
    implicit val parserToTest = this.oper
    parsing("x = 3") should equal(WqlOper("=", WqlVar("x"), WqlInt(3)))
    parsing("(x = 3)") should equal(WqlOper("=", WqlVar("x"), WqlInt(3)))

    assertFail("x = 3 x")
    assertFail("(x = 3")
  }

  they should "parse and expressions" in {
    implicit val parserToTest = this.and
    parsing("x = 2 and y = 3") should equal(WqlAnd(WqlOper("=", WqlVar("x"), WqlInt(2)), WqlOper("=", WqlVar("y"), WqlInt(3))))
    parsing("(x = 2) and (y = 3)") should equal(WqlAnd(WqlOper("=", WqlVar("x"), WqlInt(2)), WqlOper("=", WqlVar("y"), WqlInt(3))))
  }

  they should "parse or expressions" in {
    implicit val parserToTest = this.or
    parsing("x = 2 or y = 3") should equal(WqlOr(WqlOper("=", WqlVar("x"), WqlInt(2)), WqlOper("=", WqlVar("y"), WqlInt(3))))
    parsing("(x = 2) or (y = 3)") should equal(WqlOr(WqlOper("=", WqlVar("x"), WqlInt(2)), WqlOper("=", WqlVar("y"), WqlInt(3))))
  }

  they should "parse is null expressions" in {
    implicit val parserToTest = this.opernull
    parsing("x is null") should equal(WqlOperNull(WqlVar("x"), None))
    parsing("x is not null") should equal(WqlOperNull(WqlVar("x"), Some("not")))
  }

  they should "parse complex conditional expressions" in {
    implicit val parserToTest = this.condition
    parsing("x = 2 and (x = 4 or y = 5)") should equal(WqlAnd(WqlOper("=", WqlVar("x"), WqlInt(2)), WqlOr(WqlOper("=", WqlVar("x"), WqlInt(4)), WqlOper("=", WqlVar("y"), WqlInt(5)))))
    parsing("x = 2 and y = 3 and z = 6") should equal(WqlAnd(WqlOper("=", WqlVar("x"), WqlInt(2)), WqlAnd(WqlOper("=", WqlVar("y"), WqlInt(3)), WqlOper("=", WqlVar("z"), WqlInt(6)))))

    parsing("x = 2 and y = 3 and z = 6 and k is not null") should equal(WqlAnd(WqlOper("=", WqlVar("x"), WqlInt(2)), WqlAnd(WqlOper("=", WqlVar("y"), WqlInt(3)), WqlAnd(WqlOper("=", WqlVar("z"), WqlInt(6)), WqlOperNull(WqlVar("k"), Some("not"))))))

  }

  they should "parse where expressions" in {
    implicit val parserToTest = this.where
    parsing("where x = 2 or y = 3") should equal(WqlWhere(WqlOr(WqlOper("=", WqlVar("x"), WqlInt(2)), WqlOper("=", WqlVar("y"), WqlInt(3)))))
    parsing("where (x = 2) or (y = 3)") should equal(WqlWhere(WqlOr(WqlOper("=", WqlVar("x"), WqlInt(2)), WqlOper("=", WqlVar("y"), WqlInt(3)))))
  }

  they should "parse join expressions" in {
    implicit val parserToTest = this.join
    parsing("join users by uuid, events by uuid, rest by uuid") should equal(WqlJoin(
      List((WqlVar("users"), WqlVar("uuid")), (WqlVar("events"), WqlVar("uuid")), (WqlVar("rest"), WqlVar("uuid")))))
  }

  they should "parse filter expressions" in {
    implicit val parserToTest = this.filter
    parsing("filter users by (evid == 104)") should equal(WqlFilter(WqlVar("users"), WqlOper("==", WqlVar("evid"), WqlInt(104))))
    parsing("filter users by evid == 104") should equal(WqlFilter(WqlVar("users"), WqlOper("==", WqlVar("evid"), WqlInt(104))))
    parsing("filter users by (evid == 104) and src == 3") should equal(WqlFilter(WqlVar("users"), WqlAnd(WqlOper("==", WqlVar("evid"), WqlInt(104)), WqlOper("==", WqlVar("src"), WqlInt(3)))))
  }

}
