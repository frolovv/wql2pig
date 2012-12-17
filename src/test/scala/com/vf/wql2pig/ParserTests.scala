package com.vf.wql2pig

import org.hamcrest.Matchers.is
import org.hamcrest.MatcherAssert._
import org.junit.Test

/**
 * User: valeryf
 * Date: 12/17/12 6:09 PM
 */
class ParserTests {

  val selectSamples = List(
    ("select evid from users;", SelectExpr(ColumnsExpr(List("evid")), VarExpr("users"), EmptyWhereExpr(), EmptyOrder())),
    ("select evid, uuid from users;", SelectExpr(ColumnsExpr(List("evid", "uuid")), VarExpr("users"), EmptyWhereExpr(), EmptyOrder())),
    ("select * from users;", SelectExpr(AllColumnsExpr(), VarExpr("users"), EmptyWhereExpr(), EmptyOrder()))
  )

  @Test def runSelects() {
    runSamples(selectSamples)
  }

  def runSamples(samples: List[(String, Expr)]) = {
    for ((input, expected) <- samples) {
      val actual = Parser.parse(input)
      assertThat(actual, is(expected))
    }

  }

}
