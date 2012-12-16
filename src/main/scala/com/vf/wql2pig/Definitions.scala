package com.vf.wql2pig

abstract sealed class Token

case class IntToken(value: Int) extends Token

case class StringToken(value: String) extends Token

case class LeftParen() extends Token

case class RightParen() extends Token

case class EqSign() extends Token