package com.vf.pig.parser

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import com.vf.pig.definitions.{PigVar, PigDump}

/**
 * User: valeryf
 * Date: 12/22/12 2:36 PM
 */
class Pig2StringTests extends Pig2String with ShouldMatchers with FlatSpec{
  def parse(expr : com.vf.pig.definitions.Pig): String = {
    exprToString(expr)
  }

  "Pig2String" should "print dump expressions" in {
    exprToString(PigDump(PigVar("events"))) should equal("dump events;")
  }



}
