abstract class Expr

case class Number(val value: Double) extends Expr
case class Variable(val variable: String) extends Expr

abstract class BinaryOp(val left: Expr, val right: Expr) extends Expr

case class AddOp(val l: Expr, val r: Expr) extends BinaryOp(l, r)
case class SubtractOp(val l: Expr, val r: Expr) extends BinaryOp(l, r)
case class MultiplyOp(val l: Expr, val r: Expr) extends BinaryOp(l, r)
case class DivideOp(val l: Expr, val r: Expr) extends BinaryOp(l, r)

object Simplify {
  def simplify(exp: Expr): Expr = {
    exp match {
      case AddOp(le, re) => {
        (le, re) match {
          case (Number(0), Variable(s)) => Variable(s)
          case (Variable(s), Number(0)) => Variable(s)
        }
      }
    }
  }
  
  def main(args: Array[String]) {
    println(simplify(AddOp(Number(1), Variable("x"))))
  }
}