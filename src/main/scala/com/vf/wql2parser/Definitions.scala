package com.vf.wql2parser

/**
 * User: valeryf
 * Date: 12/16/12 7:11 PM
 */
abstract sealed class Token

case class IntToken(value : Int) extends Token
case class StringToken(value : String) extends Token
case class LeftParen() extends Token
case class RightParen() extends Token
case class EqSign() extends Token