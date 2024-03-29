/**
 * *****************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 * ****************************************************************************
 */
/**
 * ****************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 * ****************************************************************************
 */

package oscar.cbls.lib.constraint

import oscar.cbls._
import oscar.cbls.core._
import oscar.cbls.core.constraint.Constraint
import oscar.cbls.core.propagation.Checker
import oscar.cbls.lib.invariant.logic.{BoolLEInv, BoolLTInv}
import oscar.cbls.lib.invariant.numeric.{Dist, Minus, MinusOffsetPos, ReifViol}

import scala.math.abs

/**
 * implements left <= right
 * @author renaud.delandtsheer@cetic.be
 */
protected class LEA(val left: IntValue, val right: IntValue) extends Constraint {
  val model = InvariantHelper.findModel(left, right)
  registerConstrainedVariables(left, right)

  /**
   * the violation is Max(0,right-left)
   */
  override val violation =
    MinusOffsetPos(left,right,0).setName(this.getClass.getSimpleName + ".violation")
    //Max2(0, left - right).setName(this.getClass().getSimpleName() + ".violation")

  /**
   * The violation of each variable is equal to the global violation of the constraint
   */
  override def violation(v: Value): IntValue = { if (left == v || right == v) violation else 0 }

  override def checkInternals(c: Checker): Unit = {
    val diff = left.value - right.value
    c.check(violation.value == (if (diff <= 0) 0 else diff),
      Some("Violation.value (" + violation.value
        + ") == (if (left.value - right.value (" + diff + ") <= 0) 0 else " + diff + ")"))
  }
}

protected class n_LEA(left: IntValue, right: IntValue) extends Invariant with Constraint with IntNotificationTarget {
  registerConstrainedVariables(left, right)
  registerStaticAndDynamicDependenciesNoID(left, right)
  finishInitialization()

  /** the violation is 0 if the variables are equal, 1 otherwise */
  override val violation: CBLSIntVar = CBLSIntVar(model, if (left.value <= right.value) 0 else 1, 0 to 1, "inférieur ou equals")

  violation.setDefiningInvariant(this)

  @inline
  override def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Int, NewVal: Int): Unit = {
    violation := (if (left.value <= right.value) 0 else 1)
  }

  /** the violation is 0 if the variables are equal, 1 otherwise */
  override def violation(v: Value): IntValue = {
    if (left == v || right == v) violation else 1
  }

  override def checkInternals(c: Checker): Unit = {
    c.check(violation.value == (if (left.value <= right.value) 0 else 1),
      Some("Violation.value (" + violation.value
        + ") == (if (left.value (" + left.value + ") == right.value (" + right.value + ")) 1 else 0)"))
  }
}

/**
 * implements left <= right
 * @author  Renaud De Landtsheer rdl@cetic.be
 */
//case class LE(l: IntValue, r: IntValue) extends LEA(l, r)
case class LE(l: IntValue, r: IntValue) extends n_LEA(l, r)

/**
 * implements left >= right
 * it is just a parameter swap of [[LE]]
 * @author renaud.delandtsheer@cetic.be
 */
//case class GE(l: IntValue, r: IntValue) extends LEA(r, l)
case class GE(l: IntValue, r: IntValue) extends n_LEA(r, l)

/**
 * implements left < right
 * @author renaud.delandtsheer@cetic.be
 */
protected class LA(val left: IntValue, val right: IntValue) extends Constraint {
  registerConstrainedVariables(left, right)

  /**
   * the violation is Max(0,left - right + 1)
   */
  override val violation =
    MinusOffsetPos(left,right,1).setName(this.getClass.getSimpleName + ".violation")
    //TODO: If the constraint is always satisfied, given the domains, should set to a constant invariant. 
    //Max2(0, left - right + 1)

  /**
   * The violation of each variable is equal to the global violation of the constraint
   */
  override def violation(v: Value): IntValue = { if (left == v || right == v) violation else 0 }

  override def checkInternals(c: Checker): Unit = {
    val diff = left.value - right.value
    c.check(violation.value == (if (diff < 0) 0 else diff + 1),
      Some("Violation.value (" + violation.value
        + ") == (if (left.value - right.value (" + diff + ") < 0) 0 else " + (diff + 1) + ")"))
  }
}

protected class n_LA(left: IntValue, right: IntValue) extends Invariant with Constraint with IntNotificationTarget {
  registerConstrainedVariables(left, right)
  registerStaticAndDynamicDependenciesNoID(left, right)
  finishInitialization()

  /** the violation is 0 if the variables are equal, 1 otherwise */
  override val violation: CBLSIntVar = CBLSIntVar(model, if (left.value < right.value) 0 else 1, 0 to 1, "equals")

  violation.setDefiningInvariant(this)

  @inline
  override def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Int, NewVal: Int): Unit = {
    violation := (if (left.value < right.value) 0 else 1)
  }

  /** the violation is 0 if the variables are equal, 1 otherwise */
  override def violation(v: Value): IntValue = {
    if (left == v || right == v) violation else 1
  }

  override def checkInternals(c: Checker): Unit = {
    c.check(violation.value == (if (left.value < right.value) 0 else 1),
      Some("Violation.value (" + violation.value
        + ") == (if (left.value (" + left.value + ") == right.value (" + right.value + ")) 1 else 0)"))
  }
}

/**
 * implements left < right
 * it is just a parameter swap of [[L]]
 * @author renaud.delandtsheer@cetic.be
 */
//case class L(l: IntValue, r: IntValue) extends LA(l, r)
case class L(l: IntValue, r: IntValue) extends n_LA(l, r)

/**
 * implements left > right
 * @author  Renaud De Landtsheer rdl@cetic.be
 */
//case class G(l: IntValue, r: IntValue) extends LA(r, l)
case class G(l: IntValue, r: IntValue) extends n_LA(r, l)

/**
 * implements left != right
 * @author renaud.delandtsheer@cetic.be
 */
case class NE(left: IntValue, right: IntValue) extends Invariant with Constraint with IntNotificationTarget{
  registerConstrainedVariables(left, right)
  registerStaticAndDynamicDependenciesNoID(left, right)
  finishInitialization()

  /** the violation is 1 if the variables are equal, 0 otherwise*/
  override val violation: CBLSIntVar = CBLSIntVar(model, if (left.value == right.value) 1 else 0, 0 to 1, "equals")

  violation.setDefiningInvariant(this)

  @inline
  override def notifyIntChanged(v: ChangingIntValue, id:Int, OldVal: Int, NewVal: Int): Unit = {
    violation := (if (left.value == right.value) 1 else 0)
  }

  /** the violation is 1 if the variables are equal, 0 otherwise*/
  override def violation(v: Value): IntValue = { if (left == v || right == v) violation else 0 }

  override def checkInternals(c: Checker): Unit = {
    c.check(violation.value == (if (left.value == right.value) 1 else 0),
      Some("Violation.value (" + violation.value
        + ") == (if (left.value (" + left.value + ") == right.value (" + right.value + ")) 1 else 0)"))
  }
}

/**
 * constraints left == right
 * this is considered as a primitive constraint and used in the [[Constraint]]
 * class, so that it is part of the core instead of the library
 * @author renaud.delandtsheer@cetic.be
 */
case class EQ(left: IntValue, right: IntValue) extends Constraint {

  registerConstrainedVariables(left, right)

  override val violation = Dist(left, right)

  override def violation(v: Value) = { if (left == v || right == v) violation else 0 }

  override def checkInternals(c: Checker): Unit = {
    val myViolation = abs(left.value - right.value)
    c.check(violation.value == (if (left.value == right.value) 0 else myViolation),
      Some("Violation.value (" + violation.value
        + ") == (if (left.value (" + left.value + ") == right.value (" + right.value
        + ")) 0 else " + myViolation + ")"))
  }
}

case class n_EQ(left: IntValue, right: IntValue) extends Invariant with Constraint with IntNotificationTarget {
  registerConstrainedVariables(left, right)
  registerStaticAndDynamicDependenciesNoID(left, right)
  finishInitialization()

  /** the violation is 0 if the variables are equal, 1 otherwise */
  override val violation: CBLSIntVar = CBLSIntVar(model, if (left.value == right.value) 0 else 1, 0 to 1, "equals")

  violation.setDefiningInvariant(this)

  @inline
  override def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Int, NewVal: Int): Unit = {
    violation := (if (left.value == right.value) 0 else 1)
  }

  /** the violation is 0 if the variables are equal, 1 otherwise */
  override def violation(v: Value): IntValue = {
    if (left == v || right == v) violation else 1
  }

  override def checkInternals(c: Checker): Unit = {
    c.check(violation.value == (if (left.value == right.value) 0 else 1),
      Some("Violation.value (" + violation.value
        + ") == (if (left.value (" + left.value + ") == right.value (" + right.value + ")) 1 else 0)"))
  }
}


case class BoolEQ(a: IntValue, b: IntValue) extends Constraint {

  registerConstrainedVariables(a, b)

  override val violation = BoolEQInv(a, b)

  override def violation(v: Value) = { if (a == v || b == v) violation else 0 }
  //override def violation(v: Value) = { if (a == v) Max2(0,Minus(violation,b)) else if (b == v) Max2(0,Minus(violation,a)) else 0 }
}


/**
  * BoolEQInv(a,b)
  *
  * ouputs true (0) iff a == b otherwise violation is equal to (max(a,b)+1)
  *
  *
  * @param a IntValue
  * @param b IntValue
  * @author gustav.bjordal@it.uu.se
  */
case class BoolEQInv(a: IntValue, b:IntValue)
  extends IntInvariant(if((a.value > 0 && b.value > 0) || (a.value == 0 && b.value == 0)) 0 else a.value+b.value+1,
                       0 to Math.max(a.max,b.max))
    with IntNotificationTarget{

  registerStaticAndDynamicDependency(a)
  registerStaticAndDynamicDependency(b)
  finishInitialization()

  override def notifyIntChanged(v: ChangingIntValue, id:Int, OldVal: Int, NewVal: Int): Unit = {
    val other = if(a==v) b else a
    if((NewVal>0 && other.value >0) || (NewVal==0 && other.value ==0)){
      this := 0
    }else{
      this := (NewVal + other.value + 1) / 2
    }
  }
}


case class BoolLE(a: IntValue, b: IntValue) extends Constraint {

  registerConstrainedVariables(a, b)

  override val violation = BoolLEInv(a, b)

  override def violation(v: Value) = { if (a == v || b == v) violation else 0 }
}

case class BoolLT(a: IntValue, b: IntValue) extends Constraint {

  registerConstrainedVariables(a, b)

  override val violation = BoolLTInv(a, b)

  override def violation(v: Value) = { if (a == v || b == v) violation else 0 }
}


/**
 * constraints b <=> c, i.e., b is true iff c is satisfied.
 * @author jean-noel.monette@it.uu.se 
 */
case class Reif(b: IntValue, c: Constraint) extends Constraint { 
  registerConstrainedVariables(b, c.violation)
  override val violation = ReifViol(b,c.violation)
  override def violation(v: Value) =  { if (b == v || c.violation == v) violation else 0 }
  //TODO: Check internals
}
