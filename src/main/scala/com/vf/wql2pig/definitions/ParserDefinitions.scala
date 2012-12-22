package com.vf.wql2pig.definitions

/**
 * User: valeryf
 * Date: 12/17/12 1:26 AM
 */
abstract sealed class WqlExpr

case class VarWqlExpr(name: String) extends WqlExpr

abstract sealed class LiteralWqlExpr extends WqlExpr

case class StringWqlExpr(str: String) extends LiteralWqlExpr

case class IntWqlExpr(num: Int) extends LiteralWqlExpr

case class BooleanWqlExpr(value: Boolean) extends LiteralWqlExpr

case class AssignWqlExpr(name: VarWqlExpr, expr: WqlExpr) extends WqlExpr

case class SelectWqlExpr(columns: ColumnsWqlExpr, from: VarWqlExpr, where: AbstractWhereWqlExpr, order: AbstractOrder) extends WqlExpr

case class ColumnsWqlExpr(names: List[String]) extends WqlExpr

abstract sealed class AbstractWhereWqlExpr extends WqlExpr

case class WhereWqlExpr(condition : ConditionWqlExpr) extends AbstractWhereWqlExpr

case class EmptyWhereWqlExpr() extends AbstractWhereWqlExpr

abstract sealed class AbstractOrder extends WqlExpr

case class EmptyOrder() extends AbstractOrder

case class SelectOrderWqlExpr(orders: List[(VarWqlExpr, OrderWqlExpr)]) extends AbstractOrder

case class FullOrderWqlExpr(table: VarWqlExpr, orders: List[(VarWqlExpr, OrderWqlExpr)]) extends AbstractOrder

case class OrderWqlExpr(ordr : String) extends WqlExpr

abstract sealed class ConditionWqlExpr() extends WqlExpr

case class AndWqlExpr(left : ConditionWqlExpr, right : ConditionWqlExpr) extends ConditionWqlExpr

case class OrWqlExpr(left : ConditionWqlExpr, right : ConditionWqlExpr) extends ConditionWqlExpr

case class OperWqlExpr(oper: String, left: WqlExpr, right: WqlExpr) extends ConditionWqlExpr

case class JoinWqlExpr(tablesAndColumns : List[(VarWqlExpr, VarWqlExpr)]) extends WqlExpr

case class FilterWqlExpr(relation : VarWqlExpr, conditions : ConditionWqlExpr) extends WqlExpr