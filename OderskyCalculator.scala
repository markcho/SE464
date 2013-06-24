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
    def eval(): Exp
  }
  
  trait Num extends Exp {
    var value: Double = _
    
    def eval(): Exp = Num(value)
  }
  
  trait Var extends Exp {
    var term: String = _
    
    def eval(): Exp = Var(term)
  }
}

/**
 * OPERATORS
 */

trait BasePlus extends Base {
  trait Plus extends Exp {
    var left: exp = _
    var right: exp = _
    
    override def eval() = {
      (left.eval(), right.eval()) match {
        case (l: Num, r: Num) => Num(l.value + r.value)
        case _ => this
      }
    }
  }
}

trait BaseNeg extends Base {
  trait Neg extends Exp {
    var expr: exp = _
    
    override def eval() = {
      expr.eval() match {
        case e: Num => Num(-e.value)
        case _ => this
      }
    }
  }
}

trait BaseMult extends Base {
  trait Mult extends Exp {
    var left: exp = _
    var right: exp = _
    
    override def eval() = {
      (left.eval(), right.eval()) match {
        case (l: Num, r: Num) => Num(l.value * r.value)
        case _ => this
      }
    }
  }
}

trait BaseDiv extends Base {
  trait Div extends Exp {
    var left: exp = _
    var right: exp = _
    
    override def eval() = {
      (left.eval(), right.eval()) match {
        case (l: Num, r: Num) => Num(l.value / r.value)
        case _ => this
      }
    }
  }
}

trait BaseOperators extends BasePlus with BaseNeg with BaseDiv with BaseMult {
  def Plus(l: exp, r: exp): exp
  def Neg(e: exp): exp
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
  
  protected def pb(n: exp): String = {
    n match {
      case v: Num => v.print()
      case v: Var => v.print()
      case _ => "(" + n.print() + ")"
    }
  }
  
  trait Plus extends super.Plus with Exp {
    override def print(): String = pb(left) + " + " + pb(right)
  }
  
  trait Neg extends super.Neg with Exp {
    override def print(): String = "-" + pb(expr)
  }
  
  trait Mult extends super.Mult with Exp {
    override def print(): String = pb(left) + " * " + pb(right)
  }
  
  trait Div extends super.Div with Exp {
    override def print(): String = pb(left) + " / " + pb(right)
  }
}

/**
 * IMPLEMENTATION
 */

object OderskyCalculator extends OperatorsPrinter {
  implicit def numToNum(d: Double): Num = Num(d)
  implicit def strToVar(s: String): Var = Var(s)
  
  type exp = Exp
  
  def Num(v: Double) = new Num { value = v }
  def Var(t: String) = new Var { term = t }
  def Neg(e: exp) = new Neg { expr = e }
  def Plus(l: exp, r: exp) = new Plus { left = l; right = r }
  def Mult(l: exp, r: exp) = new Mult { left = l; right = r }
  def Div(l: exp, r: exp) = new Div { left = l; right = r }
  
  def runTests() {
    val tests = List[Exp](
        Plus(Neg(Plus(1, 2)), 6),
        Mult(Plus(Neg(1), 5), Neg(Plus(4, 5))),
        Div(25, Plus(2, 3)),
        Plus(10, Plus(Plus("x", "y"), 3))
    )
    
    val expr: Exp = "x".eval()
    
    tests.foreach { test =>
      println("=========================")
      println("EVAL:    " + test.eval())
      println("PRINT:   " + test.print())
      println("=========================")
    }
  }
}