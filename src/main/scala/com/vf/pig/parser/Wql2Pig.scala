package com.vf.pig.parser

import com.vf.pig.definitions._
import com.vf.pig.definitions.PigAssign
import com.vf.pig.definitions.PigVar
import com.vf.pig.definitions.PigForeach
import com.vf.wql.definitions
import com.vf.pig.definitions.PigForeach
import definitions._
import com.vf.pig.definitions.PigSchema
import com.vf.pig.definitions.PigAssign
import com.vf.pig.definitions.PigVar
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

  def createSchema(names: List[String]): PigSchema = {
    val types = names map {
      case x if List("evid", "date_created").contains(x) => "long"
      case _ => "chararray"
    }
    PigSchema(names, types)
  }

  def emitSelect(columns: List[String], table: String, where: WqlAbstractWhere, order: WqlAbstractOrder, relation: String) : List[Pig] = {
    var result =  new ListBuffer[Pig]()
    result += PigForeach(PigVar(table), columns, createSchema(columns))
    where match {
      case WqlWhere(condition) => {
        result += PigAssign(PigVar(relation), PigFilter(PigVar(relation), wql2pig(condition)))
      }
    }
    order match {
      case WqlSelectOrder(orders) => {
        val mapped = orders map {case (WqlVar(column), WqlDirection(direction)) => (PigVar(column), PigDirection(direction))}
        result += PigAssign(PigVar(relation), PigOrder(PigVar(relation), mapped , PigParallel(3)))
      }
    }

    result.toList
  }

  def wql2pig(expr: WqlExpr): Pig = {
    expr match {
      case WqlAnd(left, right) => PigAnd(wql2pig(left).asInstanceOf[PigCondition], wql2pig(right).asInstanceOf[PigCondition])
      case WqlOr(left, right) => PigOr(wql2pig(left).asInstanceOf[PigCondition], wql2pig(right).asInstanceOf[PigCondition])
      case WqlOper(oper, left, right) => PigOper(oper, wql2pig(left), wql2pig(right))
    }
  }

  def wql2pigs(wql: WqlExpr): List[Pig] = {
    wql match {

      case WqlAssign(WqlVar(relation), WqlSelect(columns, WqlVar(table), where, order)) => {
        emitSelect(columns,table, where, order, relation) match {
          case head :: Nil => PigAssign(PigVar(relation), head) :: Nil
          case head :: rest => PigAssign(PigVar(relation), head) :: rest
        }
      }
      case WqlAssign(WqlVar(name), expr: WqlExpr) => {
        PigAssign(PigVar(name), wql2pig(expr)) :: Nil
      }
    }
  }
}
