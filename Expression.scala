abstract class Expr

case class Number(val value: Double) extends Expr
case class Variable(val variable: String) extends Expr

abstract class BinaryOp(val left: Expr, val right: Expr) extends Expr
  
case class AddOp(val l: Expr, val r: Expr) extends BinaryOp(l, r)
case class SubtractOp(val l: Expr, val r: Expr) extends BinaryOp(l, r)
case class MultiplyOp(val l: Expr, val r: Expr) extends BinaryOp(l, r)
case class DivideOp(val l: Expr, val r: Expr) extends BinaryOp(l, r)


object Calculator {
  def eval(exp: Expr): Double = {
    exp match {
      case Number(v) => v
      case Variable(v) => 999

      case AddOp(Number(l), Number(r)) => l + r
      case SubtractOp(Number(l), Number(r)) => l - r
      case MultiplyOp(Number(l), Number(r)) => l * r
      case DivideOp(Number(l), Number(r)) => l / r
    }
  }

  def main(args: Array[String]): Unit = {
    println("Running Test Cases")

    val testCases = Map(
      AddOp(Number(1), Number(2)) -> 3,
      AddOp(Number(2), Number(3)) -> 5
    )

    //testCases.map(pair => eval(pair._1) -> pair._2).foreach(println)
    testCases map { pair =>
      eval(pair._1) -> pair._2
    } filter { pair =>
      pair._1 != pair._2
    } foreach { pair =>
      println("Expected: " + pair._2 + " | Calculated: " + pair._1)
    }

    println("Completed Test Cases")
  }
}
