package com.vf.pig.parser

import com.vf.pig.definitions._
/**
 * User: valeryf
 * Date: 12/22/12 2:26 PM
 */
trait Pig2String {
  val semicolumn = ";"

  def exprToString(expr: Pig): String = {
    expr match {
      case PigDump(PigVar(name)) => "dump " + name + semicolumn
      case PigDescribe(PigVar(name)) => "describe " + name + semicolumn
      case PigFilter(PigVar(name), condition: Pig) => {
        "filter " + name + " by " + exprToString(condition)
      }
      case Equality() => "=="
      case ColumnFilterEquality() => "="
      case PigField(name) => name
      case PigNumber(value) => value.toString
      case PigSymbolic(value) => "'" + value + "'"
      case PigVar(name) => name
      case PigEmptyCondition() => ""


      case PigAssign(PigVar(name), value) => name + " = " + exprToString(value) + semicolumn

      case PigAnd(exprs : List[PigCondition]) => {
        exprs match {
          case Nil => ""
          case e :: Nil => exprToString(e)
          case _ => {
            val mapped = exprs map exprToString
            val joined = mapped.mkString(") and (")
            "(" + joined + ")"
          }
        }
      }
      case PigOr(exprs : List[PigCondition]) => {
        exprs match {
          case Nil => ""
          case e :: Nil => exprToString(e)
          case _ => {
            val mapped = exprs map exprToString
            val joined = mapped.mkString(") or (")
            "(" + joined + ")"
          }
        }
      }
      case PigOper(operator, field, value) =>
        exprToString(field) + " " + exprToString(operator) + " " + exprToString(value)

      case PigGroup(PigVar(name), exprs, par) => exprs match {
        case PigField(field) :: Nil => "group " + name + " by " + field + " " + exprToString(par)
        case exprs2 => {
          val mapped = exprs2 map exprToString
          val joined = mapped.mkString(", ")
          "(" + joined + ")"
        }
      }

      case PigLoad(from: PigSymbolic, udf: AbstractPigUdf, schema: PigSchema) => {
        "load " + exprToString(from) + " using " + exprToString(udf) + " as " + exprToString(schema)
      }
      case PigStore(PigVar(name), dir, udf) => {
        "store " + name + " into " + exprToString(dir) + " using " + exprToString(udf) + ";"
      }

      case PigDistinct(PigVar(name), par) => "distinct " + name + exprToString(par)
      case PigLimit(PigVar(name), n) => "limit " + name + " " + n
      case Order(PigVar(name), orders, par) => {
        val mapped = orders map exprToString
        val joined = mapped.mkString(", ")

        "order " + name + " by " + joined + " " + exprToString(par)
      }

      case PigOrderPair(PigField(name), PigOrderDesc()) => name + " " + "desc"
      case PigOrderPair(PigField(name), PigOrderAsc()) => name + " " + "asc"

      case PigUnion(first, second, rest) => {
        val mapped = (first :: second :: rest) map exprToString
        val joined = mapped.mkString(", ")
        "union " + joined
      }

      case PigParallel(1) => ""
      case PigParallel(cnt) => "parallel " + cnt

      case PigUdf(name, exprs) => {
        val values = exprs map exprToString
        val quoted = values map quote
        name + "(" + quoted.mkString(",\n\t") + ")"
      }

      case PigKeyFilter(start, stop, src) => {
        "date_created between (\"" + start + "\", \"" + stop + "\") and src = " + src
      }

      case PigColumnFilter(conditions) => {
        exprToString(conditions)
      }

      case PigWixTableLoader(table, keyFilter, columnFilter, columns) => {
        val mapped = columns map {case x : String => "event:" + x}
        val joined = mapped.mkString(" ")

        exprToString(PigUdf("TableLoader", List(PigSymbolic(table), keyFilter, columnFilter, PigSymbolic(joined))))
      }

      case PigSchema(names, types) =>
        val zip = names zip types

        val concat = zip map {
          case (n, t) => n + ":" + t
        }

        "(" + concat.mkString(",") + ")"
    }
  }

  def exprsToString(exprs: List[Pig]): String = {
    val values = exprs map exprToString
    values.mkString("\n")
  }

  def quote(str : String ) : String = {
    if(str.startsWith("'") && str.endsWith("'"))
      str
    else
      "'" + str + "'"
  }
}
