package com.vf.wql2pig

/**
 * User: valeryf
 * Date: 12/17/12 1:26 AM
 */
abstract sealed class Expr

case class VarExpr(name: String) extends Expr

abstract sealed class LiteralExpr extends Expr

case class StringExpr(str: String) extends LiteralExpr

case class IntExpr(num: Int) extends LiteralExpr

case class BooleanExpr(value: Boolean) extends LiteralExpr

case class AssignExpr(name: VarExpr, expr: Expr) extends Expr

case class SelectExpr(columns: ColumnsExpr, from: VarExpr, where: AbstractWhereExpr, order: AbstractOrder) extends Expr

case class ColumnsExpr(names: List[String]) extends Expr

abstract sealed class AbstractWhereExpr extends Expr

case class WhereExpr(condition : ConditionExpr) extends AbstractWhereExpr

case class EmptyWhereExpr() extends AbstractWhereExpr

abstract sealed class AbstractOrder extends Expr

case class EmptyOrder() extends AbstractOrder

case class SelectOrderExpr(orders: List[(VarExpr, OrderExpr)]) extends AbstractOrder

case class FullOrderExpr(table: VarExpr, orders: List[(VarExpr, OrderExpr)]) extends AbstractOrder

case class OrderExpr(ordr : String) extends Expr

abstract sealed class ConditionExpr() extends Expr

case class AndExpr(left : ConditionExpr, right : ConditionExpr) extends ConditionExpr

case class OrExpr(left : ConditionExpr, right : ConditionExpr) extends ConditionExpr

case class OperExpr(oper: String, left: Expr, right: Expr) extends ConditionExpr

case class JoinExpr(tablesAndColumns : List[(VarExpr, VarExpr)]) extends Expr