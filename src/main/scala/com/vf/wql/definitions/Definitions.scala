package com.vf.wql.definitions

/**
 * User: valeryf
 * Date: 12/17/12 1:26 AM
 */
abstract sealed class WqlExpr

case class WqlVar(name: String) extends WqlExpr

abstract sealed class LiteralWqlExpr extends WqlExpr

case class WqlString(str: String) extends LiteralWqlExpr

case class WqlNull() extends LiteralWqlExpr

case class WqlInt(value: Int) extends LiteralWqlExpr

case class WqlBoolean(value: Boolean) extends LiteralWqlExpr

case class WqlAssign(name: WqlVar, expr: WqlExpr) extends WqlExpr

abstract sealed class WqlAbstractSelect() extends WqlExpr

case class WqlSelect(columns: List[String], from: WqlVar) extends WqlAbstractSelect

case class WqlSelectWithWhere(select: WqlAbstractSelect, where: WqlWhere) extends WqlAbstractSelect

case class WqlSelectWithOrder(select: WqlAbstractSelect, order: WqlSelectOrder) extends WqlAbstractSelect

case class WqlSelectWithGroup(select: WqlAbstractSelect, group: WqlGroup) extends WqlAbstractSelect

case class WqlSelectWithTBL(select: WqlAbstractSelect, wherekey: WqlWhereKey) extends WqlAbstractSelect

abstract sealed class WqlAbstractWhere extends WqlExpr

case class WqlWhere(condition: WqlCondition) extends WqlAbstractWhere

case class WqlWhereKey(src: String, start: String, end: String) extends WqlAbstractWhere

abstract sealed class WqlAbstractOrder extends WqlExpr

case class WqlSelectOrder(orders: List[(WqlVar, WqlDirection)]) extends WqlAbstractOrder

case class WqlFullOrder(table: WqlVar, orders: List[(WqlVar, WqlDirection)]) extends WqlAbstractOrder

case class WqlDirection(direction: String) extends WqlExpr

abstract sealed class WqlCondition() extends WqlExpr

case class WqlAnd(left: WqlCondition, right: WqlCondition) extends WqlCondition

case class WqlOr(left: WqlCondition, right: WqlCondition) extends WqlCondition

case class WqlOper(oper: String, left: WqlExpr, right: WqlExpr) extends WqlCondition

case class WqlOperNull(what: WqlExpr, isnull: Option[String]) extends WqlCondition

case class WqlJoin(tablesAndColumns: List[(WqlVar, WqlVar)]) extends WqlExpr

case class WqlFilter(relation: WqlVar, conditions: WqlCondition) extends WqlExpr

abstract sealed class WqlAbstractGroup extends WqlExpr

case class WqlGroup(fields: List[WqlVar]) extends WqlAbstractGroup

case class WqlFunc(name: String, args: List[WqlExpr]) extends WqlExpr