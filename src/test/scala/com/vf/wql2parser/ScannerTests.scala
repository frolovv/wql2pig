package com.vf.wql2parser

import org.hamcrest.Matchers.is
import org.hamcrest.MatcherAssert.assertThat
import org.junit.Test

/**
 * User: valeryf
 * Date: 12/17/12 12:47 AM
 */
class ScannerTests {

  val intTokens = List(
    ("123", List(IntToken(123)))
  )

  val stringTokens = List(
    ("abc", List(StringToken("abc")))
  )

  val complexTests = List(
    ("anon = select client_id, segment from anonymous_by_src \nwhere evid = 1000 and src = 19",
      List(StringToken("anon"), EqSign(), StringToken("select"), StringToken("client_id"),
        StringToken("segment"), StringToken("from"), StringToken("anonymous_by_src"),
        StringToken("where"), StringToken("evid"), EqSign(), IntToken(1000),
        StringToken("and"), StringToken("src"), EqSign(), IntToken(19)))
  )


  @Test def runComplexTests() {
    testSamples(complexTests)
  }

  @Test def runIntTokens() {
    testSamples(intTokens)
  }

  @Test def runStringTokens() {
    testSamples(stringTokens)
  }


  def testSamples(samples: List[(String, List[Token])]) {
    for ((input, expected) <- samples) {
      val actual = Scanner.scan(input)
      assertThat(actual, is(expected))
    }
  }
}
