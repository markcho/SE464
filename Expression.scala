abstract class Expr

case class Number(val value: Double) extends Expr
case class Variable(val variable: String) extends Expr

abstract class BinaryOp(val left: Expr, val right: Expr) extends Expr

case class AddOp(val l: Expr, val r: Expr) extends BinaryOp(l, r)
case class SubtractOp(val l: Expr, val r: Expr) extends BinaryOp(l, r)
case class MultiplyOp(val l: Expr, val r: Expr) extends BinaryOp(l, r)
case class DivideOp(val l: Expr, val r: Expr) extends BinaryOp(l, r)

object Calculator {
  def simplify(exp: Expr): Expr = {
    def swap(mat: (Expr, Expr)): (Expr, Expr) = {
      mat match {
        case (l: Expr, r: Variable) => (r, l)
        case _ => mat
      }
    }
    
    exp match {
      case AddOp(le, re) => {
        swap(simplify(le), simplify(re)) match {
          case (Variable(s), Number(0)) => Variable(s)
          case (Variable(s1), Variable(s2)) if s1 == s2 => MultiplyOp(Variable(s1), Number(2))
          case (MultiplyOp(l1, r1), MultiplyOp(l2, r2)) => {
            (swap(l1, r1), swap(l2, r2)) match {
              case ((Variable(l11), r11: Expr), (Variable(l12), r12: Expr)) if l11 == l12 => MultiplyOp(AddOp(r11, r12), Variable(l11))
              case _ => exp
            }
          }
          case _ => exp
        }
      }
      case _ => exp
    }
  }
  
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
    println("Running Simplify Test Cases")

    val testCases = Map(
      AddOp(Number(0), Variable("x")) -> Variable("x"),
      AddOp(Variable("x"), Number(0)) -> Variable("x")
    )

    //testCases.map(pair => eval(pair._1) -> pair._2).foreach(println)
    testCases map { pair =>
      simplify(pair._1) -> pair._2
    } filter { pair =>
      pair._1 != pair._2
    } foreach { pair =>
      println("Expected: " + pair._2 + " | Calculated: " + pair._1)
    }

    println("Completed Test Cases")
    
//    println("Running Test Cases")
//
//    val testCases = Map(
//      AddOp(Number(1), Number(2)) -> 3,
//      AddOp(Number(2), Number(3)) -> 5
//    )
//
//    //testCases.map(pair => eval(pair._1) -> pair._2).foreach(println)
//    testCases map { pair =>
//      eval(pair._1) -> pair._2
//    } filter { pair =>
//      pair._1 != pair._2
//    } foreach { pair =>
//      println("Expected: " + pair._2 + " | Calculated: " + pair._1)
//    }
//
//    println("Completed Test Cases")
  }
}
