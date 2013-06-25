package se464.calculator

import scala.language.implicitConversions

/**
 * BASE
 */

trait Base {
  type exp <: Exp
  
  def Num(d: Double): exp
  def Var(s: String): exp
  
  trait Exp {
    def toNum(): Double
    def substitute(values: Map[String, Double] = Map()): exp
  }
  
  trait Num extends Exp {
    var value: Double = _
    
    override def toNum() = value
    override def substitute(values: Map[String, Double] = Map()) = Num(value)
  }
  
  trait Var extends Exp {
    var term: String = _
    
    override def toNum() = 0
    override def substitute(values: Map[String, Double] = Map()) = {
      values.get(term) match {
        case Some(v) => Num(v)
        case None => Var(term)
      }
    }
  }
}




/**
 * OPERATORS
 */

trait BaseOperators extends Base {
  type exp <: Exp
  
  trait Binary extends Exp {
    var left: exp = _
    var right: exp = _
  }
  
  trait Plus extends Binary {
    override def toNum() = left.toNum() + right.toNum()
    override def substitute(values: Map[String, Double] = Map()) =
        Plus(left.substitute(values), right.substitute(values))
  }
  
  trait Sub extends Binary {
    override def toNum() = left.toNum() - right.toNum()
    override def substitute(values: Map[String, Double] = Map()) =
        Sub(left.substitute(values), right.substitute(values))
  }
  
  trait Mult extends Binary {
    override def toNum() = left.toNum() * right.toNum()
    override def substitute(values: Map[String, Double] = Map()) =
        Mult(left.substitute(values), right.substitute(values))
  }
  
  trait Div extends Binary {
    override def toNum() = left.toNum() / right.toNum()
    override def substitute(values: Map[String, Double] = Map()) =
        Div(left.substitute(values), right.substitute(values))
  }
  
  def Plus(l: exp, r: exp): exp
  def Sub(l: exp, r: exp): exp
  def Mult(l: exp, r: exp): exp
  def Div(l: exp, r: exp): exp
}




/**
 * PRINTERS
 */

trait BasePrinter extends Base {
  type exp <: Exp
  
  trait Exp extends super.Exp {
    def print(): String
  }
  
  trait Num extends super.Num with Exp {
    override def print(): String = value.toString()
  }
  
  trait Var extends super.Var with Exp {
    override def print(): String = term
  }
}

trait OperatorsPrinter extends BaseOperators with BasePrinter {
  type exp <: super[BasePrinter].Exp
  
  private def pb(n: exp): String = {
    n match {
      case b: Binary => "(" + b.print() + ")"
      case v => v.print()
    }
  }
  
  trait Plus extends super.Plus with Exp {
    override def print(): String = pb(left) + " + " + pb(right)
  }
  
  trait Sub extends super.Sub with Exp {
    override def print(): String = pb(left) + " - " + pb(right)
  }
  
  trait Mult extends super.Mult with Exp {
    override def print(): String = pb(left) + " * " + pb(right)
  }
  
  trait Div extends super.Div with Exp {
    override def print(): String = pb(left) + " / " + pb(right)
  }
}



/**
 * VARIABLES
 */

trait BaseVariable extends BasePrinter {
  type exp <: Exp
  
  trait Exp extends super.Exp {
    def eval(): exp
  }
  
  trait Num extends super.Num with Exp {
    override def eval() = Num(value)
  }
  
  trait Var extends super.Var with Exp {
    override def eval() = Var(term)
  }
}

trait OperatorsVariable extends OperatorsPrinter with BaseVariable {
  type exp <: Exp
  
  trait Binary extends super.Binary with Exp {
    def binaryCompress(arg: (exp, exp) => exp): exp = {
        (left.eval(), right.eval()) match {
          case (l: Num, r: Num) => Num(toNum())
          case v => arg(v._1, v._2)
        }
    }
  }
  
  trait Plus extends super.Plus with Binary {
    override def eval() = binaryCompress(Plus(_, _))
  }
  
  trait Sub extends super.Sub with Binary {
    override def eval() = binaryCompress(Sub(_, _))
  }
  
  trait Mult extends super.Mult with Binary {
    override def eval() = binaryCompress(Mult(_, _))
  }
  
  trait Div extends super.Div with Binary {
    override def eval() = binaryCompress(Div(_, _))
  }
}



/**
 * SIMPLIFIER
 */

trait BaseSimplifier extends OperatorsVariable {
  type exp <: Exp
  
  trait Exp extends super.Exp {
    def simplify(): exp
  }
  
  trait Num extends super.Num with Exp {
    override def simplify() = Num(value)
  }
  
  trait Var extends super.Var with Exp {
    override def simplify() = Var(term)
  }
  
  trait Binary extends super.Binary with Exp {
    def swap(): (exp, exp) = {
      (left.simplify(), right.simplify()) match {
        case (l, r: Var) => (Var(r.term), l)
        case (l, r) => (l, r)
      }
    }
  }
  
  trait Plus extends super.Plus with Binary {
    override def simplify() = {
      swap() match {
        case (l: Var, r: Num) if r.value == 0 => Var(l.term)
        case (l: Var, r: Var) if l.term == r.term => Mult(Var(l.term), Num(2))
        case (l: Mult, r: Mult) => {
          (l.swap(), r.swap()) match {
            case ((l1: Var, r1), (l2: Var, r2)) if l1.term == l2.term =>
              Mult(Plus(r1, r2).simplify(), Var(l1.term))
            case v => Plus(Mult(v._1._1, v._1._2), Mult(v._2._1, v._2._2))
          }
        }
        case v => Plus(v._1, v._2)
      }
    }
  }
  
  trait Sub extends super.Sub with Binary {
    override def simplify() = {
      (left.simplify(), right.simplify()) match {
        case (l: Var, r: Var) if l.term == r.term => Num(0)
        case v => Sub(v._1, v._2)
      }
    }
  }
  
  trait Mult extends super.Mult with Binary {
    override def simplify() = {
      swap() match {
        case (l: Var, r: Num) if r.value == 1 => Var(l.term)
        case v => Mult(v._1, v._2)
      }
    }
  }
  
  trait Div extends super.Div with Binary {
    override def simplify() = {
      (left.simplify(), right.simplify()) match {
        case (l: Var, r: Num) if r.value == 1 => Var(l.term)
        case (l: Mult, r: Var) => {
          l.swap() match {
            case (l1: Var, r1) if r.term == l1.term => r1
            case v => Div(Mult(v._1, v._2), Var(r.term))
          }
        }
        case v => Div(v._1, v._2)
      }
    }
  }
}



/**
 * IMPLEMENTATION
 */

object OderskyCalculator extends BaseSimplifier {
  implicit def numToNum(d: Double): Num = Num(d)
  implicit def strToVar(s: String): Var = Var(s)
  
  type exp = Exp
  
  def Num(v: Double) = new Num { value = v }
  def Var(t: String) = new Var { term = t }
  def Plus(l: exp, r: exp) = new Plus { left = l; right = r }
  def Sub(l: exp, r: exp) = new Sub { left = l; right = r }
  def Mult(l: exp, r: exp) = new Mult { left = l; right = r }
  def Div(l: exp, r: exp) = new Div { left = l; right = r }
  
  def main(args: Array[String]): Unit = {
    val tests = List[(Exp, Map[String, Double])](
        (Plus(Plus(1, 2), 6), Map()),
        (Mult(Plus(1, 5), Div(Plus(4, 5), 3)), Map()),
        (Sub(25, Plus(25, 80)), Map()),
        (Div(25, Plus(2, 3)), Map()),
        (Plus(10, Mult(Plus("x", "y"), Plus(3, 25))), Map("x" -> 2.5, "y" -> 8))
    )
    
    tests.foreach { test =>
      println("=========================")
      println("SUBSTITUTE:  " + test._1.substitute(test._2).eval().toNum())
      println("EVAL:        " + test._1.eval().print())
      println("PRINT:       " + test._1.print())
      println("=========================")
    }
    
    val simplifyTestCases: List[(exp, exp)] = List(
      Plus(0, "x") -> "x",
      Plus("x", 0) -> "x",
      Plus("y", "y") -> Mult("y", 2),
      Plus(Mult("y", 2), Mult(8, "y")) -> Mult(Plus(2.0, 8.0), "y"),
      Sub("x", "x") -> 0,
      Div(Mult("y", 2), "y") -> 2,
      Div("x", 1) -> "x",
      Div(1, "x") -> Div(1, "x"),
      Mult(1, "x") -> "x",
      Mult("x", 1) -> "x",
      Plus(Mult("y", Plus("x", 0)), Mult(Sub("y", "y"), "y")) -> Mult("x", "y")
    )

    val evaluateTestCases = List[(exp, exp)](
      Plus(1,2) -> 3,
      Plus(2,-3) -> -1,
      Sub(3,5) -> -2,
      Sub(3,-2) -> 5,
      Mult(5,2) -> 10,
      Mult(10,1) -> 10,
      Mult(10,0.2) -> 2,
      Div(10,4) -> 2.5,
      Div(5,-1) -> -5,

      Plus(Plus(Plus(1, 2), 3), Plus(4, 5)) -> 15
    )

    val variableTestCases = List[(exp, exp)](
      Plus("x", 5) -> 6,
      Plus(3, "w") -> 2,
      Mult("w", 5) -> -5,
      Mult("z", "y") -> 10,
      Plus("q", 5) -> Plus("q", 5),
      Mult(Sub("q", "e"), "x") -> Mult(Sub("q", "e"), 1)
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
      pair._1.simplify().print() -> pair._2.print()
    } filter { pair =>
      pair._1 != pair._2
    } foreach { pair =>
      println("Expected: " + pair._2 + " | Calculated: " + pair._1)
    }

    println("===========================")
    println("Running Evaluate Test Cases")


    evaluateTestCases map { pair =>
      pair._1.eval().print() -> pair._2.print()
    } filter { pair =>
      pair._1 != pair._2
    } foreach { pair =>
      println("Expected: " + pair._2 + " | Calculated: " + pair._1)
    }

    println("===========================")
    println("Running Variable Test Cases")


    variableTestCases map { pair =>
      pair._1.substitute(variableMapping).eval().print() -> pair._2.print()
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