package com.vf.wql2pig

import util.parsing.input.CharSequenceReader
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import com.vf.wql2pig.definitions._

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
    parsing("true") should equal(BooleanWqlExpr(value = true))
    parsing("false") should equal(BooleanWqlExpr(value = false))
    assertFail("True")
    assertFail("False")
    assertFail("TRUE")
    assertFail("FALSE")
    assertFail("truefoo")
  }

  they should "parse numeric literals" in {
    implicit val parserToTest = this.integer
    parsing("123") should equal(IntWqlExpr(123))
    parsing("-123") should equal(IntWqlExpr(-123))
    assertFail("T123")
    assertFail("F123alse")
  }

  they should "parse string literals" in {
    implicit val parserToTest = this.string
    parsing("'abc'") should equal(StringWqlExpr("abc"))
    parsing("'users'") should equal(StringWqlExpr("users"))
    assertFail("T123")
    assertFail("F123alse")
    assertFail("\"F123alse")
  }

  they should "parse identifier literals" in {
    implicit val parserToTest = this.ident
    parsing("abc") should equal(VarWqlExpr("abc"))
    parsing("users") should equal(VarWqlExpr("users"))
    //parsing("users and more") should  equal(VarWqlExpr("users"))
    parsing(" users ") should equal(VarWqlExpr("users"))

    assertFail("'T123")
    assertFail("'F123alse")
  }

}
