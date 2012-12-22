package com.vf.pig.definitions

/**
 * User: valeryf
 * Date: 12/22/12 2:25 PM
 */
sealed abstract class Pig

case class PigAssign(name: PigVar, value: Pig) extends Pig

case class PigFilter(what: PigVar, condition: Pig) extends Pig

case class PigLoad(what: PigSymbolic, using: AbstractPigUdf, as: Pig) extends Pig

sealed abstract class AbstractPigUdf extends Pig

case class PigUdf(name: String, exprs: List[Pig]) extends AbstractPigUdf

case class PigSymbolic(value: String) extends Pig

case class PigSchema(names: List[String], types: List[String]) extends Pig

case class PigVar(name: String) extends Pig

case class PigGroup(what: PigVar, exprs: List[PigField], parallel: PigParallel) extends Pig

case class PigParallel(cnt: Int) extends Pig

case class PigDump(what: PigVar) extends Pig

sealed class PigCondition extends Pig

case class PigAnd(exprs : List[Pig]) extends PigCondition

case class PigOr(exprs : List[Pig]) extends PigCondition

case class PigOper(operator: Pig, field: Pig, value: Pig) extends PigCondition

case class PigEmptyCondition() extends PigCondition

case class Equality() extends Pig
case class ColumnFilterEquality() extends Pig

case class PigField(name: String) extends Pig

case class PigNumber(value: Int) extends Pig

case class PigDistinct(what: PigVar, par: PigParallel) extends Pig

case class PigLimit(what: PigVar, cnt: Int) extends Pig

sealed abstract class AbstractPigOrder extends Pig

case class PigOrderAsc() extends AbstractPigOrder

case class PigOrderDesc() extends AbstractPigOrder

case class PigOrderPair(field: PigField, order: AbstractPigOrder) extends Pig

case class Order(what: PigVar, orders: List[PigOrderPair], par: PigParallel) extends Pig

case class PigUnion(first: PigVar, second: PigVar, rest: List[PigVar]) extends Pig

case class PigDescribe(what: PigVar) extends Pig

case class PigStore(what: PigVar, where: PigSymbolic, udf: AbstractPigUdf) extends Pig

case class PigWixTableLoader(table: String, keyFilter: PigKeyFilter,
                             columnFilter: PigColumnFilter,
                             columns: List[String]) extends AbstractPigUdf

case class PigKeyFilter(start: String, stop: String, src: Int) extends Pig

case class PigColumnFilter(conditions : PigCondition) extends Pig
