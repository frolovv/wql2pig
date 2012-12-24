package com.vf.pig.definitions

/**
 * User: valeryf
 * Date: 12/22/12 2:25 PM
 */
sealed abstract class Pig

case class PigAssign(name: PigVar, value: Pig) extends Pig

case class PigFilter(what: PigVar, condition: Pig) extends Pig

case class PigLoad(what: PigVar, using: AbstractPigUdf, as: Pig) extends Pig

sealed abstract class AbstractPigUdf extends Pig

case class PigUdf(name: String, exprs: List[Pig]) extends AbstractPigUdf

case class PigSchema(names: List[String], types: List[String]) extends Pig

case class PigVar(name: String) extends Pig

case class PigGroup(what: PigVar, exprs: List[String], parallel: PigParallel) extends Pig

case class PigParallel(cnt: Int) extends Pig

case class PigDump(what: PigVar) extends Pig

sealed class PigCondition extends Pig

case class PigAnd(left : PigCondition, right : PigCondition) extends PigCondition

case class PigOr(left : PigCondition, right : PigCondition) extends PigCondition

case class PigOper(oper: String, field: Pig, value: Pig) extends PigCondition

case class PigEmptyCondition() extends PigCondition

case class Equality() extends Pig
case class ColumnFilterEquality() extends Pig

case class PigInt(value: Int) extends Pig

case class PigDistinct(what: PigVar, par: PigParallel) extends Pig

case class PigLimit(what: PigVar, cnt: Int) extends Pig

case class PigDirection(direction : String) extends Pig

case class PigOrder(what: PigVar, orders: List[(PigVar, PigDirection)], par: PigParallel) extends Pig

case class PigUnion(first: PigVar, second: PigVar, rest: List[PigVar]) extends Pig

case class PigDescribe(what: PigVar) extends Pig

case class PigStore(what: PigVar, where: PigVar, udf: AbstractPigUdf) extends Pig

case class PigWixTableLoader(table: String, keyFilter: PigKeyFilter,
                             columnFilter: PigColumnFilter,
                             columns: List[String]) extends AbstractPigUdf

case class PigKeyFilter(start: String, stop: String, src: Int) extends Pig

case class PigColumnFilter(conditions : PigCondition) extends Pig

case class PigForeach(table : PigVar, columns : List[String], as : PigSchema) extends Pig

case class PigJoin(tablesAndColumns : List[(PigVar, PigVar)]) extends Pig