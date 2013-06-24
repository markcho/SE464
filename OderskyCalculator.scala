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
    def eval(): Double
  }
  
  trait Num extends Exp {
    var value: Double = _
    
    def eval() = value
  }
}

/**
 * OPERATORS
 */

trait BaseOperators extends Base {
  type exp <: Exp
  
  trait Neg extends Exp {
    var expr: exp = _
    
    override def eval() = -expr.eval()
  }
  
  trait Binary extends Exp {
    var left: exp = _
    var right: exp = _
  }
  
  trait Plus extends Binary {
    override def eval() = left.eval() + right.eval()
  }
  
  trait Sub extends Binary {
    override def eval() = left.eval() - right.eval()
  }
  
  trait Mult extends Binary {
    override def eval() = left.eval() * right.eval()
  }
  
  trait Div extends Binary {
    override def eval() = left.eval() / right.eval()
  }
  
  def Neg(e: exp): exp
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
}

trait OperatorsPrinter extends BaseOperators with BasePrinter {
  type exp <: super[BasePrinter].Exp
  
  protected def pb(n: exp): String = {
    n match {
      case b: Binary => "(" + b.print() + ")"
      case v => v.print()
    }
  }
  
  trait Neg extends super.Neg with Exp {
    override def print(): String = "-" + pb(expr)
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
  
  def Num(d: Double): exp
  def Var(s: String): exp
  
  trait Exp extends super.Exp {
    def compress(): exp
  }
  
  trait Num extends Exp with super.Num {
    override def compress() = Num(value)
  }
  
  trait Var extends Exp {
    var term: String = _
    
    override def print(): String = term
    override def compress() = Var(term)
    override def eval() = 0
  }
}

trait OperatorsVariable extends OperatorsPrinter with BaseVariable {
  type exp <: Exp
  
  trait Neg extends super.Neg with Exp {
    override def compress() = {
      expr.compress() match {
        case e: Num => Num(-e.value)
        case v => Neg(v)
      }
    }
  }
  
  trait Plus extends super.Plus with Exp {
    override def compress(): exp = {
      (left.compress(), right.compress()) match {
        case (l: Num, r: Num) => Num(eval())
        case v => Plus(v._1, v._2)
      }
    }
  }
  
  trait Sub extends super.Sub with Exp {
    override def compress(): exp = {
      (left.compress(), right.compress()) match {
        case (l: Num, r: Num) => Num(eval())
        case v => Sub(v._1, v._2)
      }
    }
  }
  
  trait Mult extends super.Mult with Exp {
    override def compress() = {
      (left.compress(), right.compress()) match {
        case (l: Num, r: Num) => Num(eval())
        case v => Mult(v._1, v._2)
      }
    }
  }
  
  trait Div extends super.Div with Exp {
    override def compress() = {
      (left.compress(), right.compress()) match {
        case (l: Num, r: Num) => Num(eval())
        case v => Div(v._1, v._2)
      }
    }
  }
}

/**
 * IMPLEMENTATION
 */

object OderskyCalculator extends OperatorsVariable {
  implicit def numToNum(d: Double): Num = Num(d)
  implicit def strToVar(s: String): Var = Var(s)
  
  type exp = Exp
  
  def Num(v: Double) = new Num { value = v }
  def Var(t: String) = new Var { term = t }
  def Neg(e: exp) = new Neg { expr = e }
  def Plus(l: exp, r: exp) = new Plus { left = l; right = r }
  def Sub(l: exp, r: exp) = new Sub { left = l; right = r }
  def Mult(l: exp, r: exp) = new Mult { left = l; right = r }
  def Div(l: exp, r: exp) = new Div { left = l; right = r }
  
  def main(args: Array[String]): Unit = {
    val tests = List[Exp](
        Plus(Neg(Plus(1, 2)), 6),
        Mult(Plus(Neg(1), 5), Neg(Plus(4, 5))),
        Sub(25, Plus(Neg(25), 80)),
        Div(25, Plus(2, 3)),
        Plus(10, Plus(Plus("x", "y"), Plus(3, 25)))
    )
    
    
    tests.foreach { test =>
      println("=========================")
      println("EVAL:        " + test.eval())
      println("COMPRESS:    " + test.compress().print())
      println("PRINT:       " + test.print())
      println("=========================")
    }
  }
}