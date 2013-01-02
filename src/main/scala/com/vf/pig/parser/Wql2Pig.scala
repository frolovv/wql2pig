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

  private def emitSelect(columns: List[String], table: String, whereKey: WqlAbstractWhere, where: WqlAbstractWhere, order: WqlAbstractOrder, relation: String): List[Pig] = {
    var result = new ListBuffer[Pig]()

    whereKey match {
      case WqlWhereKey(src, start, end) =>
        result += PigLoad(PigVar("wix-bi"),
          PigWixTableLoader(table,
            PigKeyFilter(start, end, src.toInt),
            PigColumnFilter(pigify(where).asInstanceOf[PigCondition]), columns),
          createSchema(columns))
      case WqlEmptyWhere() => {
        result += PigForeach(PigVar(table), columns, createSchema(columns))

        where match {
          case WqlWhere(condition) => {
            result += PigAssign(PigVar(relation), PigFilter(PigVar(relation), pigify(condition)))
          }
          case _ => {}
        }
      }
    }


    order match {
      case WqlSelectOrder(orders) => {
        val mapped = orders map {
          case (WqlVar(column), WqlDirection(direction)) => (PigVar(column), PigDirection(direction))
        }
        result += PigAssign(PigVar(relation), PigOrder(PigVar(relation), mapped, PigParallel(3)))
      }
      case _ => {}
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
      case WqlAssign(WqlVar(relation), WqlSelect(columns, WqlVar(table), whereKey, where, group, order)) :: rest => {
        emitSelect(columns, table, whereKey, where, order, relation) match {
          case head :: Nil => PigAssign(PigVar(relation), head) :: pigify(rest)
          case head :: rest2 => PigAssign(PigVar(relation), head) :: rest2 ++ pigify(rest)
        }
      }
      case WqlAssign(WqlVar(name), expr: WqlExpr) :: rest => {
        PigAssign(PigVar(name), pigify(expr)) :: pigify(rest)
      }
    }
  }
}
