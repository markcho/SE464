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
        val sw = swap(simplify(le), simplify(re))
        lazy val se = AddOp(sw._1, sw._2)
            
        sw match {
          case (Variable(s), Number(0)) => s
          case (Variable(s1), Variable(s2)) if s1 == s2 => MultiplyOp(s1, 2)
          case (MultiplyOp(l1, r1), MultiplyOp(l2, r2)) => {
            (swap(l1, r1), swap(l2, r2)) match {
              case ((Variable(l11), r11: Expr), (Variable(l12), r12: Expr)) if l11 == l12 => MultiplyOp(simplify(AddOp(r11, r12)), l11)
              case _ => se
            }
          }
          case _ => se
        }
      }
      case SubtractOp(le, re) => {
        val sw = swap(simplify(le), simplify(re))
        lazy val se = SubtractOp(sw._1, sw._2)
        
        sw match {
          case (Variable(s1), Variable(s2)) if s1 == s2 => 0
          case _ => se
        }
      }
      case MultiplyOp(le, re) => {
        val sw = swap(simplify(le), simplify(re))
        lazy val se = MultiplyOp(sw._1, sw._2)
            
        sw match {
          case (Variable(s1), Number(1)) => s1
          case _ => se
        }
      }
      case DivideOp(le, re) => {
        val sw = (simplify(le), simplify(re))
        lazy val se = DivideOp(sw._1, sw._2)
            
        sw match {
          case (Variable(s1), Number(1)) => s1
          case (MultiplyOp(l1, r1), Variable(s1)) => {
            (swap(l1, r1)) match {
              case (Variable(l11), r11: Expr) if l11 == s1 => r11
              case _ => se
            }
          }
          case _ => se
        }
      }
      case _ => exp
    }
  }

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
    val testCases1: List[(Expr, Expr)] = List(
      AddOp(0, "x") -> "x",
      AddOp("x", 0) -> "x",
      AddOp("y", "y") -> MultiplyOp("y", 2),
      AddOp(MultiplyOp("y", 2), MultiplyOp(8, "y")) -> MultiplyOp(AddOp(2.0, 8.0), "y"),
      SubtractOp("x", "x") -> 0,
      DivideOp(MultiplyOp("y", 2), "y") -> 2,
      DivideOp("x", 1) -> "x",
      DivideOp(1, "x") -> DivideOp(1, "x"),
      MultiplyOp(1, "x") -> "x",
      MultiplyOp("x", 1) -> "x",
      AddOp(MultiplyOp("y", AddOp("x", 0)), MultiplyOp(SubtractOp("y", "y"), "y")) -> MultiplyOp("x", "y")
    )

    println("===========================")
    println("Running Simplify Test Cases")

    testCases1 map { pair =>
      simplify(pair._1) -> pair._2
    } filter { pair =>
      pair._1 != pair._2
    } foreach { pair =>
      println("Expected: " + pair._2 + " | Calculated: " + pair._1)
    }

    println("===========================")
    println("Running Evaluate Test Cases")

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

    println("===========================")
    println("Completed Test Cases")
    println("===========================")
  }
}

