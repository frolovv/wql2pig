package com.vf.wql.parser

import util.parsing.input.CharSequenceReader
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import com.vf.wql.definitions._

/**
 * User: valeryf
 * Date: 12/18/12 1:14 AM
 */
class WqlConstantsTests extends WqlConstants with ShouldMatchers with FlatSpec {

  private def parsing[T](s: String)(implicit p: Parser[T]): T = {
    //wrap the parser in the phrase parse to make sure all input is consumed
    val phraseParser = phrase(p)
    //we need to wrap the string in a reader so our parser can digest it
    val input = new CharSequenceReader(s)
    phraseParser(input) match {
      case Success(t, _) => t
      case NoSuccess(msg, _) => throw new IllegalArgumentException(
        "Could not parse '" + s + "': " + msg)
    }
  }

  private def assertFail[T](input: String)(implicit p: Parser[T]) {
    evaluating(parsing(input)(p)) should produce[IllegalArgumentException]
  }

  "The WqlConstants" should "parse boolean literals" in {
    implicit val parserToTest = boolean
    parsing("true") should equal(WqlBoolean(value = true))
    parsing("false") should equal(WqlBoolean(value = false))
    assertFail("True")
    assertFail("False")
    assertFail("TRUE")
    assertFail("FALSE")
    assertFail("truefoo")
  }

  they should "parse numeric literals" in {
    implicit val parserToTest = this.integer
    parsing("123") should equal(WqlInt(123))
    parsing("-123") should equal(WqlInt(-123))
    assertFail("T123")
    assertFail("F123alse")
  }

  they should "parse string literals" in {
    implicit val parserToTest = this.string
    parsing("'abc'") should equal(WqlString("abc"))
    parsing("'users'") should equal(WqlString("users"))
    assertFail("T123")
    assertFail("F123alse")
    assertFail("\"F123alse")
  }

  they should "parse identifier literals" in {
    implicit val parserToTest = this.ident
    parsing("abc") should equal(WqlVar("abc"))
    parsing("users") should equal(WqlVar("users"))
    //parsing("users and more") should  equal(WqlVar("users"))
    parsing(" users ") should equal(WqlVar("users"))

    assertFail("'T123")
    assertFail("'F123alse")
  }

}
