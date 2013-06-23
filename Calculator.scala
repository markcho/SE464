package se464.calculator

import scala.language.implicitConversions

object Calculator {
  implicit def doubleToNumber(x: Double) = Number(x)
  implicit def stringToVariable(x: String) = Variable(x)

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
          case (Variable(s), Number(0)) => s
          case (Variable(s1), Variable(s2)) if s1 == s2 => MultiplyOp(s1, 2)
          case (MultiplyOp(l1, r1), MultiplyOp(l2, r2)) => {
            (swap(l1, r1), swap(l2, r2)) match {
              case ((Variable(l11), r11: Expr), (Variable(l12), r12: Expr)) if l11 == l12 => MultiplyOp(AddOp(r11, r12), l11)
              case _ => exp
            }
          }
          case _ => exp
        }
      }
      case SubtractOp(le, re) => {
        swap(simplify(le), simplify(re)) match {
          case (Variable(s1), Variable(s2)) if s1 == s2 => 0
          case _ => exp
        }
      }
      case MultiplyOp(le, re) => {
        swap(simplify(le), simplify(re)) match {
          case (Variable(s1), Number(1)) => s1
          case _ => exp
        }
      }
      case DivideOp(le, re) => {
        (simplify(le), simplify(re)) match {
          case (Variable(s1), Number(1)) => s1
          case (MultiplyOp(l1, r1), Variable(s1)) => {
            (swap(l1, r1)) match {
              case (Variable(l11), r11: Expr) if l11 == s1 => r11
              case _ => exp
            }
          }
          case _ => exp
        }
      }
      case _ => exp
    }
  }

  def eval(exp: Expr, variables: Map[String, Double] = Map[String, Double]()): Double = {
    exp match {
      case Number(v) => v
      case Variable(v) => variables.get(v).get
      case AddOp(left, right) => eval(left, variables) + eval(right, variables)
      case SubtractOp(left, right) => eval(left, variables) - eval(right, variables)
      case MultiplyOp(left, right) => eval(left, variables) * eval(right, variables)
      case DivideOp(left, right) => eval(left, variables) / eval(right, variables)
    }
  }

  def main(args: Array[String]): Unit = {

    val simplifyTestCases: List[(Expr, Expr)] = List(
      AddOp(Number(0), Variable("x")) -> Variable("x"),
      AddOp(Variable("x"), Number(0)) -> Variable("x"),
      AddOp(Variable("y"), Variable("y")) -> MultiplyOp(Variable("y"),Number(2)),
      AddOp(MultiplyOp("y", 2), MultiplyOp(8, "y")) -> MultiplyOp(AddOp(2.0, 8.0), "y"),
      DivideOp(MultiplyOp("y", 2), "y") -> 2,
      DivideOp("x", 1) -> "x",
      DivideOp(1, "x") -> DivideOp(1, "x"),
      MultiplyOp(1, "x") -> "x",
      MultiplyOp("x", 1) -> "x"
    )

    val evaluateTestCases = List(
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

    val variableTestCases = List(
      AddOp("x", 5) -> 6,
      AddOp(3, "w") -> 2,
      MultiplyOp("w", 5) -> -5,
      MultiplyOp("z", "y") -> 10
    )

    val variableMapping = Map[String, Double](
      "w" -> -1,
      "x" -> 1,
      "y" -> 2,
      "z" -> 5
    )

    println("===========================")
    println("Running Simplify Test Cases")

    simplifyTestCases map { pair =>
      simplify(pair._1) -> pair._2
    } filter { pair =>
      pair._1 != pair._2
    } foreach { pair =>
      println("Expected: " + pair._2 + " | Calculated: " + pair._1)
    }

    println("===========================")
    println("Running Evaluate Test Cases")


    evaluateTestCases map { pair =>
      eval(pair._1) -> pair._2
    } filter { pair =>
      pair._1 != pair._2
    } foreach { pair =>
      println("Expected: " + pair._2 + " | Calculated: " + pair._1)
    }

    println("===========================")
    println("Running Variable Test Cases")


    variableTestCases map { pair =>
      eval(pair._1, variableMapping) -> pair._2
    } filter { pair =>
      pair._1 != pair._2
    } foreach { pair =>
      println("Expected: " + pair._2 + " | Calculated: " + pair._1)
    }

    println("===========================")
    println("Completed Test Cases")
    println("===========================")
  }
}
