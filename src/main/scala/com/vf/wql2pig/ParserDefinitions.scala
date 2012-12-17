package com.vf.wql2pig

/**
 * User: valeryf
 * Date: 12/17/12 1:26 AM
 */
abstract sealed class Expr

case class VarExpr(name: String) extends Expr

case class StringExpr(str: String) extends Expr

case class IntExpr(num: Int) extends Expr

case class AssignExpr(name: VarExpr, expr: Expr) extends Expr

case class SelectExpr(columns: AbstractColumnsExpr, from: VarExpr, where: AbstractWhereExpr, order: AbstractOrder) extends Expr

abstract sealed class AbstractColumnsExpr extends Expr
case class ColumnsExpr(names: List[String]) extends AbstractColumnsExpr
case class AllColumnsExpr() extends AbstractColumnsExpr

abstract sealed class AbstractWhereExpr extends Expr

case class WhereExpr(conditions: List[ConditionExpr]) extends AbstractWhereExpr

case class EmptyWhereExpr() extends AbstractWhereExpr

abstract sealed class AbstractOrder extends Expr

case class EmptyOrder() extends AbstractOrder

case class OrderExpr(orders: List[(VarExpr, SimpleOrderExpr)]) extends AbstractOrder

case class ConditionExpr(oper: Expr, left: Expr, right: Expr) extends Expr

abstract sealed class SimpleOrderExpr extends Expr

case class AscOrder() extends SimpleOrderExpr

case class DestOrder() extends SimpleOrderExpr
