package com.vf.pig.parser

import com.vf.pig.definitions._
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import com.vf.wql.parser.WqlParser
import com.vf.pig.definitions.PigVar
import com.vf.pig.definitions.PigForeach

/**
 * User: valeryf
 * Date: 12/24/12 12:38 AM
 */
class Wql2PigTests extends Wql2Pig with ShouldMatchers with FlatSpec with WqlParser {
  def parsing(line: String): List[Pig] = {
    val wqls = super.parse(line)
    pigify(wqls)
  }

  "Wql2Pig" should "pigify select statements" in {
    val assign: PigAssign = PigAssign(PigVar("x"), PigForeach(PigVar("users"), List("evid"), PigSchema(List("evid"), List("long"))))
    val filter: PigAssign = PigAssign(PigVar("x"), PigFilter(PigVar("x"), PigOper("=", PigVar("evid"), PigInt(4))))
    val order: PigAssign = PigAssign(PigVar("x"), PigOrder(PigVar("x"), List((PigVar("evid"), PigDirection("desc"))), PigParallel(3)))

    parsing("x = select evid from users") should equal(List(assign))
    parsing("x = select evid from users where evid = 4") should equal(List(assign, filter))
    parsing("x = select evid from users where evid = 4 order by evid desc") should equal(List(assign, filter, order))

    parsing("x = select evid, uuid from users") should equal(List(PigAssign(PigVar("x"), PigForeach(PigVar("users"), List("evid", "uuid"), PigSchema(List("evid", "uuid"), List("long", "chararray"))))))
  }

  it should "pigify order statements" in {
    parsing("x = order users by uuid desc") should equal(List(PigAssign(PigVar("x"), PigOrder(PigVar("users"), List((PigVar("uuid"), PigDirection("desc"))), PigParallel(3)))))
  }

  it should "pigify join statements" in {
    parsing("x = join users by uuid, premiums by uuid") should
      equal(List(PigAssign(PigVar("x"), PigJoin(List((PigVar("users"), PigVar("uuid")), (PigVar("premiums"), PigVar("uuid")))))))
  }

  it should "pigify filter statements" in {
    parsing("x = filter users by evid = 103") should
      equal(List(PigAssign(PigVar("x"), PigFilter(PigVar("users"), PigOper("=", PigVar("evid"), PigInt(103))))))
  }
}
