package com.vf.pig.parser

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import com.vf.pig.definitions._
import com.vf.pig.definitions.PigOr
import com.vf.pig.definitions.PigUdf
import com.vf.pig.definitions.PigSchema
import com.vf.pig.definitions.PigGroup
import com.vf.pig.definitions.PigVar
import com.vf.pig.definitions.PigNumber
import com.vf.pig.definitions.PigFilter
import com.vf.pig.definitions.PigOper
import com.vf.pig.definitions.PigLoad
import com.vf.pig.definitions.PigAnd
import com.vf.pig.definitions.PigParallel
import com.vf.pig.definitions.PigDump

/**
 * User: valeryf
 * Date: 12/22/12 2:36 PM
 */
class PigPrinterTests extends PigPrinter with ShouldMatchers with FlatSpec {
  def parse(expr: com.vf.pig.definitions.Pig): String = {
    exprToString(expr)
  }

  "PigPrinter" should "print dump expressions" in {
    exprToString(PigDump(PigVar("events"))) should equal("dump events;")
  }

  they should "print simple and conditions" in {
    exprToString(PigAnd(PigOper("==", PigVar("evid"), PigNumber(103)), PigOper("==", PigVar("evid"), PigNumber(103)))) should equal("(evid == 103) and (evid == 103)")
  }

  they should "print simple or conditions" in {
    exprToString(PigOr(PigOper("==", PigVar("evid"), PigNumber(103)), PigOper("==", PigVar("evid"), PigNumber(103)))) should equal("(evid == 103) or (evid == 103)")
  }

  they should "print filter statements" in {
    exprToString(PigFilter(PigVar("events"), PigAnd(PigOper("==", PigVar("evid"), PigNumber(103)), PigOper("==", PigVar("evid"), PigNumber(103))))) should equal("filter events by (evid == 103) and (evid == 103)")
  }

  they should "print load statements" in {
    exprToString(PigLoad(PigVar("wix-bi"), PigUdf("TableLoader", List(PigVar("users_by_src"))), PigSchema(List("uuid"), List("chararray")))) should
      equal("load 'wix-bi' using TableLoader('users_by_src') as (uuid:chararray)")
  }

  they should "print group statements" in {
    exprToString(PigGroup(PigVar("events"), List("evid"), PigParallel(6))) should
      equal("group events by evid parallel 6")
  }

  they should "print assign statements" in {
    exprToString(PigAssign(PigVar("events"), PigLoad(PigVar("wix-bi"), PigUdf("TableLoader", List(PigVar("users_by_src"))), PigSchema(List("uuid"), List("chararray"))))) should
      equal("events = load 'wix-bi' using TableLoader('users_by_src') as (uuid:chararray);")
  }

  they should "print limit statements" in {
    exprToString(PigLimit(PigVar("events"), 10)) should
      equal("limit events 10")
  }

  they should "print order statements" in {
    exprToString(PigOrder(PigVar("events"), List((PigVar("evid"), PigDirection("desc"))), PigParallel(3))) should
      equal("order events by evid desc parallel 3")
  }

  they should "print union statements" in {
    exprToString(PigUnion(PigVar("events_user"), PigVar("events_anon"), Nil)) should
      equal("union events_user, events_anon")
  }
  they should "print describe statements" in {
    exprToString(PigDescribe(PigVar("events"))) should
      equal("describe events;")
  }
  they should "print store statements" in {
    exprToString(PigStore(PigVar("events"), PigVar("wix-bi"), PigUdf("PigStorage", List(PigVar("\t"))))) should
      equal("store events into 'wix-bi' using PigStorage('\t');")
  }
  they should "print table loader statements" in {
    exprToString(PigWixTableLoader("users_by_src",
      PigKeyFilter("2012-12-12 00:00", "2012-12-12 01:00", 42),
      PigColumnFilter(PigOper("=", PigVar("event:evid"), PigNumber(100))),
      List("uuid", "evid"))) should
      equal("TableLoader('users_by_src',\n\t'date_created between (\"2012-12-12 00:00\", \"2012-12-12 01:00\") and src = 42',\n\t" +
        "'event:evid = 100',\n\t" + "'event:uuid event:evid')")
  }
  they should "print columnfilter statements" in {
    exprToString(PigColumnFilter(PigOper("=", PigVar("event:evid"), PigNumber(100)))) should
      equal("event:evid = 100")
  }
  they should "print keyfilter statements" in {
    exprToString(PigKeyFilter("2012-12-12 00:00", "2012-12-12 01:00", 42)) should
      equal("date_created between (\"2012-12-12 00:00\", \"2012-12-12 01:00\") and src = 42")
  }
}
