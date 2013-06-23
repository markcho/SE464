import scala.language.implicitConversions

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
    implicit def doubleToNumber(x: Double) = Number(x)
    implicit def stringToVariable(x: String) = Variable(x)

    println("====================")
    println("Running Test Cases")

    val testCases = List(
      AddOp(1,2) -> 3,
      AddOp(2,-3) -> -1,
      SubtractOp(3,5) -> -2,
      SubtractOp(3,-2) -> 5,
      MultiplyOp(5,2) -> 10,
      MultiplyOp(10,1) -> 10,
      MultiplyOp(10,0.2) -> 2,
      DivideOp(10,4) -> 2.5,
      DivideOp(5,-1) -> -5,

      AddOp(AddOp(AddOp(1, 2), 3), AddOp(4, 5)) -> 15
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
