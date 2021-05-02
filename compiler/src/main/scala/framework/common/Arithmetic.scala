package framework.common

/**
  * Arithmetic operators
  */

sealed trait OpArithmetic

case object OpPlus extends OpArithmetic { override def toString = "+" }

case object OpMinus extends OpArithmetic { override def toString = "-" }

case object OpMultiply extends OpArithmetic { override def toString = "*" }

case object OpDivide extends OpArithmetic { override def toString = "/" }

case object OpMod extends OpArithmetic { override def toString = "mod" }
