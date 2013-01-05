package com.vf.pig.parser

import com.vf.pig.definitions._
import com.vf.wql.definitions
import definitions._
import com.vf.pig.definitions.PigForeach
import definitions.WqlAssign
import com.vf.pig.definitions.PigSchema
import com.vf.pig.definitions.PigAssign
import com.vf.pig.definitions.PigVar
import definitions.WqlSelect
import definitions.WqlVar
import collection.mutable.ListBuffer

/**
 * User: valeryf
 * Date: 12/22/12 4:11 PM
 */
trait Wql2Pig {

  private def createSchema(names: List[String]): PigSchema = {
    val types = names map {
      case x if List("evid", "date_created").contains(x) => "long"
      case _ => "chararray"
    }
    PigSchema(names, types)
  }

  private def emitSelect(selectStmt: WqlAbstractSelect, relation: String): List[Pig] = {
    var result = new ListBuffer[Pig]()

    selectStmt match {
      case WqlSelect(columns, WqlVar(table)) => {
        result += PigForeach(PigVar(table), columns, createSchema(columns))
      }
      case WqlSelectWithOrder(select, WqlSelectOrder(orders)) => {
        result ++= emitSelect(select, relation)
        val directions = orders map {
          case (WqlVar(field), WqlDirection(dir)) => Pair(PigVar(field), PigDirection(dir))
        }
        result += PigOrder(PigVar(relation), directions, PigParallel(3))
      }
      case WqlSelectWithWhere(select, WqlWhere(condition)) => {
        select match {
          case s: WqlSelect => {
            result ++= emitSelect(select, relation)
            result += PigFilter(PigVar(relation), pigify(condition))
          }
          case s: WqlSelectWithGroup => {
            val from = s.select.asInstanceOf[WqlSelect].from
            val filter = PigFilter(PigVar(from.name), pigify(condition))
            val groupAndRest = emitSelect(select, relation) map {
              case PigGroup(_, exprs, par) => PigGroup(PigVar(relation), exprs, par)
              case x => x
            }

            result += filter
            result ++= groupAndRest
          }
          case s: WqlSelectWithTBL => {
            val inner = emitSelect(select, relation)
            val mapped = inner map {
              case PigLoad(what, tbl: PigWixTableLoader, as) => {
                PigLoad(what, PigWixTableLoader(tbl.table, tbl.keyFilter, PigColumnFilter(pigify(condition).asInstanceOf[PigCondition]), tbl.columns), as)
              }
              case x => x
            }
            result ++= mapped
          }
        }
      }
      case WqlSelectWithTBL(WqlSelect(columns, WqlVar(from)), WqlWhereKey(src, start, end)) => {
        result += PigLoad(PigVar("wix-bi"),
          PigWixTableLoader(from,
            PigKeyFilter(start, end, src.toInt),
            PigColumnFilter(PigEmptyCondition()), columns),
          createSchema(columns))
      }
      case WqlSelectWithGroup(select, WqlGroup(fields)) => {
        select match {
          case WqlSelect(columns, from) => {
            val grp = PigGroup(PigVar(from.name), fields map (_.name), PigParallel(3))
            val slct = WqlSelect(columns, WqlVar(relation))
            result += grp
            result ++= emitSelect(slct, relation)
          }
        }
      }
    }

    result.toList
  }

  private def pigify(expr: WqlExpr): Pig = {
    expr match {
      case WqlAnd(left, right) => PigAnd(pigify(left).asInstanceOf[PigCondition], pigify(right).asInstanceOf[PigCondition])
      case WqlOr(left, right) => PigOr(pigify(left).asInstanceOf[PigCondition], pigify(right).asInstanceOf[PigCondition])
      case WqlOper(oper, left, right) => PigOper(oper, pigify(left), pigify(right))
      case WqlOperNull(field, not) => PigOperNull(pigify(field), not)
      case WqlVar(x) => PigVar(x)
      case WqlInt(n) => PigInt(n)
      case WqlString(s) => PigString(s)
      case WqlEmptyWhere() => PigEmptyCondition()

      case WqlWhere(condition) => pigify(condition)
      case WqlJoin(tablesAndColumns) => {
        val mapped = tablesAndColumns map {
          case (WqlVar(table), WqlVar(col)) => (PigVar(table), PigVar(col))
        }
        PigJoin(mapped)
      }

      case WqlFullOrder(WqlVar(relation), orders) => {
        val mapped = orders map {
          case (WqlVar(col), WqlDirection(dir)) => (PigVar(col), PigDirection(dir))
        }
        PigOrder(PigVar(relation), mapped, PigParallel(3))
      }

      case WqlFilter(WqlVar(relation), condition) => {
        PigFilter(PigVar(relation), pigify(condition))
      }
    }
  }

  def pigify(wqls: List[WqlExpr]): List[Pig] = {
    wqls match {
      case Nil => Nil
      case WqlAssign(WqlVar(relation), select: WqlAbstractSelect) :: rest => {
        val mapped = emitSelect(select, relation) map (PigAssign(PigVar(relation), _))
        mapped ++ pigify(rest)
      }
      case WqlAssign(WqlVar(name), expr: WqlExpr) :: rest => {
        PigAssign(PigVar(name), pigify(expr)) :: pigify(rest)
      }
    }
  }
}
