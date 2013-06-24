package se464.calculator

import scala.language.implicitConversions

object Calculator {
  implicit def doubleToNumber(x: Double) = Number(x)
  implicit def stringToVariable(x: String) = Variable(x)

  def simplify(exp: Expr): Expr = {
    def swap(e1: Expr, e2: Expr): (Expr, Expr) = {
      (e1, e2) match {
        case (l: Expr, r: Variable) => (r, l)
        case _ => (e1, e2)
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
        val sw = (simplify(le), simplify(re))
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

  def evaluation(exp: Expr): Expr = {
    exp match {
      case Number(v) => Number(v)
      case Variable(v) => Variable(v)

      case bo: BinaryOp =>
        (evaluation(bo.left), evaluation(bo.right)) match {
          case (Number(left), Number(right)) =>
            bo match {
              case op: AddOp => Number(left + right)
              case op: SubtractOp => Number(left - right)
              case op: MultiplyOp => Number(left * right)
              case op: DivideOp => Number(left / right)
            }

          case (left, right) =>
            bo match {
              case op: AddOp => AddOp(left, right)
              case op: SubtractOp => SubtractOp(left, right)
              case op: MultiplyOp => MultiplyOp(left, right)
              case op: DivideOp => DivideOp(left, right)
            }
        }
    }
  }

  def replacement(exp: Expr, variables: Map[String, Double]): Expr = {
    exp match {
      case Number(v) => Number(v)
      case Variable(v) =>
        variables.get(v) map {
          Number(_)
        } getOrElse {
          Variable(v)
        }

      case AddOp(left, right) => AddOp(replacement(left, variables), replacement(right, variables))
      case SubtractOp(left, right) => SubtractOp(replacement(left, variables), replacement(right, variables))
      case MultiplyOp(left, right) => MultiplyOp(replacement(left, variables), replacement(right, variables))
      case DivideOp(left, right) => DivideOp(replacement(left, variables), replacement(right, variables))
    }
  }

  def main(args: Array[String]): Unit = {
    val simplifyTestCases: List[(Expr, Expr)] = List(
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

    val evaluateTestCases = List[(Expr, Expr)](
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

    val variableTestCases = List[(Expr, Expr)](
      AddOp("x", 5) -> 6,
      AddOp(3, "w") -> 2,
      MultiplyOp("w", 5) -> -5,
      MultiplyOp("z", "y") -> 10,
      AddOp("q", 5) -> AddOp("q", 5),
      MultiplyOp(SubtractOp("q", "w"), "x") -> MultiplyOp(SubtractOp("q", "w"), 1)
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
      evaluation(pair._1) -> pair._2
    } filter { pair =>
      pair._1 != pair._2
    } foreach { pair =>
      println("Expected: " + pair._2 + " | Calculated: " + pair._1)
    }

    println("===========================")
    println("Running Variable Test Cases")


    variableTestCases map { pair =>
      evaluation(replacement(pair._1, variableMapping)) -> pair._2
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

