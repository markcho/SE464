sealed trait Expr

case class Number(val value: Double) extends Expr
case class Variable(val variable: String) extends Expr

sealed abstract class BinaryOp(val left: Expr, val right: Expr) extends Expr

case class AddOp(val l: Expr, val r: Expr) extends BinaryOp(l, r)
case class SubtractOp(val l: Expr, val r: Expr) extends BinaryOp(l, r)
case class MultiplyOp(val l: Expr, val r: Expr) extends BinaryOp(l, r)
case class DivideOp(val l: Expr, val r: Expr) extends BinaryOp(l, r)


object Calculator {
  def eval(exp: Expr): Double = {
    exp match {
      case Number(v) => v
      case Variable(v) => 999

      case AddOp(left, right) => eval(left) + eval(right)
      case SubtractOp(left, right) => eval(left) - eval(right)
      case MultiplyOp(left, right) => eval(left) * eval(right)
      case DivideOp(left, right) => eval(left) / eval(right)
    }
  }

  def main(args: Array[String]): Unit = {
    println("====================")
    println("Running Test Cases")

    val testCases = Map(
      AddOp(Number(1), Number(2)) -> 3,
      AddOp(Number(2), Number(-3)) -> -1,
      SubtractOp(Number(3), Number(5)) -> -2,
      SubtractOp(Number(3), Number(-2)) -> 5,
      MultiplyOp(Number(5), Number(2)) -> 10,
      MultiplyOp(Number(10), Number(1)) -> 10,
      MultiplyOp(Number(10), Number(0.2)) -> 2,
      DivideOp(Number(10), Number(4)) -> 2.5,
      DivideOp(Number(5), Number(-1)) -> -5,

      AddOp(AddOp(AddOp(Number(1), Number(2)), Number(3)), AddOp(Number(4), Number(5))) -> 15
    )

    testCases map { pair =>
      eval(pair._1) -> pair._2
    } filter { pair =>
      pair._1 != pair._2
    } foreach { pair =>
      println("Expected: " + pair._2 + " | Calculated: " + pair._1)
    }

    println("Completed Test Cases")
    println("====================")
  }
}
