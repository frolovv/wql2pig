package com.vf.pig.parser

import com.vf.pig.definitions._

/**
 * User: valeryf
 * Date: 12/22/12 2:26 PM
 */
trait PigPrinter {
  private val semicolumn = ";"

  def addPrefixTo(condition: PigCondition, prefix: String): PigCondition = {
    condition match {
      case PigAnd(left, right) => PigAnd(addPrefixTo(left, prefix), addPrefixTo(right, prefix))
      case PigOr(left, right) => PigOr(addPrefixTo(left, prefix), addPrefixTo(right, prefix))
      case PigOper(oper, PigVar(x), value) => PigOper(oper, PigVar(prefix + x), value)
      case PigOper(oper, value, PigVar(x)) => PigOper(oper, value, PigVar(prefix + x))
      case PigOperNull(PigVar(x), not) => PigOperNull(PigVar(prefix + x), not)
      case PigEmptyCondition() => PigEmptyCondition()
    }
  }

  def pigToString(expr: Pig): String = {
    expr match {
      case PigDump(PigVar(name)) => "dump " + name + semicolumn
      case PigDescribe(PigVar(name)) => "describe " + name + semicolumn
      case PigFilter(PigVar(name), condition: Pig) => {
        "filter " + name + " by " + pigToString(condition)
      }
      case PigInt(value) => value.toString
      case PigString(value) => "\"" + value + "\""
      case PigVar(name) => name
      case PigEmptyCondition() => ""


      case PigAssign(PigVar(name), value) => name + " = " + pigToString(value) + semicolumn

      case PigForeach(PigVar(relation), columns, as) => {
        val values = columns map pigToString
        val generates = (values zip as.names zip as.types) map {
          case ((v, n), t) =>
            if (v.equalsIgnoreCase(n)) v
            else v + " as " + n + ":" + t
        }

        "foreach " + relation + " generate " + generates.mkString(", ")
      }

      case PigAnd(left: PigCondition, right: PigCondition) => {
        "(" + pigToString(left) + ") and (" + pigToString(right) + ")"
      }
      case PigOr(left: PigCondition, right: PigCondition) => {
        "(" + pigToString(left) + ") or (" + pigToString(right) + ")"
      }
      case PigOper(operator, field, value) =>
        pigToString(field) + " " + operator + " " + pigToString(value)

      case PigOperNull(field, Some(_)) => pigToString(field) + " is not null"
      case PigOperNull(field, None) => pigToString(field) + " is null"

      case PigGroup(PigVar(name), exprs, par) => exprs match {
        case field :: Nil => "group " + name + " by " + pigToString(field) + " " + pigToString(par)
        case _ => "group " + name + " by (" + (exprs map pigToString).mkString(", ") + ") " + pigToString(par)
      }

      case PigLoad(PigVar(from), udf: AbstractPigUdf, schema: PigSchema) => {
        "load " + quote(from) + " using " + pigToString(udf) + " as " + pigToString(schema)
      }
      case PigStore(PigVar(name), PigVar(dir), udf) => {
        "store " + name + " into " + quote(dir) + " using " + pigToString(udf) + ";"
      }

      case PigDistinct(PigVar(name), par) => "distinct " + name + pigToString(par)
      case PigLimit(PigVar(name), n) => "limit " + name + " " + n
      case PigOrder(PigVar(name), orders, par) => {
        val mapped = orders map {
          case (PigVar(column), PigDirection(direction)) => column + " " + direction
        }
        val joined = mapped.mkString(", ")

        "order " + name + " by " + joined + " " + pigToString(par)
      }

      case PigUnion(first, second, rest) => {
        val mapped = (first :: second :: rest) map pigToString
        val joined = mapped.mkString(", ")
        "union " + joined
      }

      case PigParallel(1) => ""
      case PigParallel(cnt) => "parallel " + cnt

      case PigUdf(name, exprs) => {
        val values = exprs map pigToString
        name + "(" + values.mkString(", ") + ")"
      }

      case PigJoin(tablesAndColumns) => {
        val mapped = tablesAndColumns map {
          case (t, c) => pigToString(t) + " by " + pigToString(c)
        }
        val joined = mapped.mkString(", ")
        "join " + joined
      }

      case PigKeyFilter(start, stop, src) => {
        "date_created between (\"" + start + "\", \"" + stop + "\") and src = " + src
      }

      case PigColumnFilter(conditions) => {
        pigToString(conditions)
      }

      case PigWixTableLoader(table, keyFilter, columnFilter, columns) => {
        val mapped = columns map ("event:" + _)
        val withPrefix = addPrefixTo(columnFilter.conditions, "event:")
        val args = List(PigVar(table), keyFilter, withPrefix, PigVar(mapped.mkString(" ")))

        "TableLoader('" + (args map pigToString).mkString("',\n\t'") + "')"
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
    val values = exprs map pigToString
    values.mkString("\n")
  }

  private def quote(str: String): String = {
    if (str.startsWith("'") && str.endsWith("'"))
      str
    else
      "'" + str + "'"
  }
}
