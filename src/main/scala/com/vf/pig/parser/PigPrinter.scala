package com.vf.pig.parser

import com.vf.pig.definitions._

/**
 * User: valeryf
 * Date: 12/22/12 2:26 PM
 */
trait PigPrinter {
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
      case PigNumber(value) => value.toString
      case PigVar(name) => name
      case PigEmptyCondition() => ""


      case PigAssign(PigVar(name), value) => name + " = " + exprToString(value) + semicolumn

      case PigAnd(left: PigCondition, right: PigCondition) => {
        "(" + exprToString(left) + ") and (" + exprToString(right) + ")"
      }
      case PigOr(left: PigCondition, right: PigCondition) => {
        "(" + exprToString(left) + ") or (" + exprToString(right) + ")"
      }
      case PigOper(operator, field, value) =>
        exprToString(field) + " " + operator + " " + exprToString(value)

      case PigGroup(PigVar(name), exprs, par) => exprs match {
        case field :: Nil => "group " + name + " by " + field + " " + exprToString(par)
        case _ => {
          val joined = exprs.mkString(", ")
          "(" + joined + ")"
        }
      }

      case PigLoad(PigVar(from), udf: AbstractPigUdf, schema: PigSchema) => {
        "load " + quote(from) + " using " + exprToString(udf) + " as " + exprToString(schema)
      }
      case PigStore(PigVar(name), PigVar(dir), udf) => {
        "store " + name + " into " + quote(dir) + " using " + exprToString(udf) + ";"
      }

      case PigDistinct(PigVar(name), par) => "distinct " + name + exprToString(par)
      case PigLimit(PigVar(name), n) => "limit " + name + " " + n
      case PigOrder(PigVar(name), orders, par) => {
        val mapped = orders map {
          case (PigVar(column), PigDirection(direction)) => column + " " + direction
        }
        val joined = mapped.mkString(", ")

        "order " + name + " by " + joined + " " + exprToString(par)
      }

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
        val mapped = columns map {
          case x: String => "event:" + x
        }
        val joined = mapped.mkString(" ")

        exprToString(PigUdf("TableLoader", List(PigVar(table), keyFilter, columnFilter, PigVar(joined))))
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

  def quote(str: String): String = {
    if (str.startsWith("'") && str.endsWith("'"))
      str
    else
      "'" + str + "'"
  }
}
