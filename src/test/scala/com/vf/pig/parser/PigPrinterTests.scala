package com.vf.pig.parser

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import com.vf.pig.definitions._
import com.vf.pig.definitions.PigOr
import com.vf.pig.definitions.PigUdf
import com.vf.pig.definitions.PigSchema
import com.vf.pig.definitions.PigGroup
import com.vf.pig.definitions.PigVar
import com.vf.pig.definitions.PigInt
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
  "PigPrinter" should "print dump expressions" in {
    pigToString(PigDump(PigVar("events"))) should equal("dump events;")
  }

  they should "print simple and conditions" in {
    pigToString(PigAnd(PigOper("==", PigVar("evid"), PigInt(103)), PigOper("==", PigVar("evid"), PigInt(103)))) should equal("(evid == 103) and (evid == 103)")
  }

  they should "print simple or conditions" in {
    pigToString(PigOr(PigOper("==", PigVar("evid"), PigInt(103)), PigOper("==", PigVar("evid"), PigInt(103)))) should equal("(evid == 103) or (evid == 103)")
  }

  they should "print filter statements" in {
    pigToString(PigFilter(PigVar("events"), PigAnd(PigOper("==", PigVar("evid"), PigInt(103)), PigOper("==", PigVar("evid"), PigInt(103))))) should equal("filter events by (evid == 103) and (evid == 103)")
  }

  they should "print load statements" in {
    pigToString(PigLoad(PigVar("wix-bi"), PigUdf("TableLoader", List(PigVar("users_by_src"))), PigSchema(List("uuid"), List("chararray")))) should
      equal("load 'wix-bi' using TableLoader(users_by_src) as (uuid:chararray)")
  }

  they should "print group statements" in {
    pigToString(PigGroup(PigVar("events"), List(PigVar("evid")), PigParallel(6))) should
      equal("group events by evid parallel 6")
  }

  they should "print assign statements" in {
    pigToString(PigAssign(PigVar("events"), PigLoad(PigVar("wix-bi"), PigUdf("TableLoader", List(PigVar("users_by_src"))), PigSchema(List("uuid"), List("chararray"))))) should
      equal("events = load 'wix-bi' using TableLoader(users_by_src) as (uuid:chararray);")
  }

  they should "print limit statements" in {
    pigToString(PigLimit(PigVar("events"), 10)) should
      equal("limit events 10")
  }

  they should "print order statements" in {
    pigToString(PigOrder(PigVar("events"), List((PigVar("evid"), PigDirection("desc"))), PigParallel(3))) should
      equal("order events by evid desc parallel 3")
  }

  they should "print union statements" in {
    pigToString(PigUnion(PigVar("events_user"), PigVar("events_anon"), Nil)) should
      equal("union events_user, events_anon")
  }
  they should "print describe statements" in {
    pigToString(PigDescribe(PigVar("events"))) should
      equal("describe events;")
  }
  they should "print store statements" in {
    pigToString(PigStore(PigVar("events"), PigVar("wix-bi"), PigUdf("PigStorage", List(PigVar("\t"))))) should
      equal("store events into 'wix-bi' using PigStorage(\t);")
  }
  they should "print table loader statements" in {
    pigToString(PigWixTableLoader("users_by_src",
      PigKeyFilter("2012-12-12 00:00", "2012-12-12 01:00", 42),
      PigColumnFilter(PigOper("=", PigVar("evid"), PigInt(100))),
      List("uuid", "evid"))) should
      equal("TableLoader('users_by_src',\n\t'date_created between (\"2012-12-12 00:00\", \"2012-12-12 01:00\") and src = 42',\n\t" +
        "'event:evid = 100',\n\t" + "'event:uuid event:evid')")
  }
  they should "print columnfilter statements" in {
    pigToString(PigColumnFilter(PigOper("=", PigVar("evid"), PigInt(100)))) should equal("evid = 100")
    pigToString(PigOperNull(PigVar("x"), Some("not"))) should equal("x is not null")
    pigToString(PigOperNull(PigVar("x"), None)) should equal("x is null")
  }
  they should "print keyfilter statements" in {
    pigToString(PigKeyFilter("2012-12-12 00:00", "2012-12-12 01:00", 42)) should
      equal("date_created between (\"2012-12-12 00:00\", \"2012-12-12 01:00\") and src = 42")
  }

  they should "print join statements" in {
    pigToString(PigJoin(List((PigVar("x"), PigVar("y")), (PigVar("z"), PigVar("k"))))) should
      equal("join x by y, z by k")
  }

  they should "print foreach statements" in {
    pigToString(PigForeach(PigVar("users"), List(PigVar("evid")), PigSchema(List("evid"), List("long")))) should
      equal("foreach users generate evid as (evid:long)")
  }

  they should "print string expressions" in {
    pigToString(PigString("Firefox")) should equal("\"Firefox\"")
  }
}
