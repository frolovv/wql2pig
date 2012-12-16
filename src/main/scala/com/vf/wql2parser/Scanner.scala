package com.vf.wql2parser

/**
 * User: valeryf
 * Date: 12/16/12 7:11 PM
 */
object Scanner {

  def isValidChar(ch: Char): Boolean = {
    ch.isLetterOrDigit || "_".contains(ch)
  }

  def isDelimiter(ch: Char): Boolean = " )(\n\t," contains ch

  def stringOrNumberToken(chars: List[Char]): Token = {
    chars.forall(ch => ch.isDigit) match {
      case true => IntToken(chars.reverse.mkString.toInt)
      case _ => StringToken(chars.reverse.mkString)
    }
  }

  def stChar(chars: List[Char], aggr: List[Char]): List[Token] = {
    chars match {
      case Nil => stringOrNumberToken(aggr) :: stInit(chars)
      case ch :: rest if isDelimiter(ch) => stringOrNumberToken(aggr) :: stInit(chars)
      case ch :: rest => stChar(rest, ch :: aggr)
    }

  }

  def stInit(chars: List[Char]): List[Token] = {
    chars match {
      case Nil => Nil
      case '(' :: rest => LeftParen() :: stInit(rest)
      case ')' :: rest => RightParen() :: stInit(rest)
      case '=' :: rest => EqSign() :: stInit(rest)
      case ch :: rest if isDelimiter(ch) => stInit(rest)
      case ch :: rest if isValidChar(ch) => stChar(rest, List(ch))
    }
  }

  def scan(line: String): List[Token] = {
    stInit(line.toList)
  }


}
