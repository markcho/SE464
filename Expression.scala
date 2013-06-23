package se464.calculator

sealed trait Expr

case class Number(val value: Double) extends Expr
case class Variable(val variable: String) extends Expr

sealed abstract class BinaryOp(val left: Expr, val right: Expr) extends Expr

case class AddOp(val l: Expr, val r: Expr) extends BinaryOp(l, r)
case class SubtractOp(val l: Expr, val r: Expr) extends BinaryOp(l, r)
case class MultiplyOp(val l: Expr, val r: Expr) extends BinaryOp(l, r)
case class DivideOp(val l: Expr, val r: Expr) extends BinaryOp(l, r)

