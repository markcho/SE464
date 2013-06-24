package se464.calculator

import scala.language.implicitConversions
import com.sun.media.sound.Printer

trait Base {
  type exp <: Exp
  
  trait Exp {
    def eval(): Double
  }
  
  trait Num extends Exp {
    var value: Double = _
    
    def eval(): Double = value
  }
}

trait BasePlus extends Base {
  trait Plus extends Exp {
    var left: exp = _
    var right: exp = _
    
    override def eval() = left.eval() + right.eval()
  }
}

trait BaseNeg extends Base {
  trait Neg extends Exp {
    var expr: exp = _
    
    override def eval() = -expr.eval()
  }
}

trait BasePlusNeg extends BasePlus with BaseNeg {
  def Num(d: Double): exp
  def Plus(l: exp, r: exp): exp
  def Neg(e: exp): exp
}

trait BasePrinter extends Base {
  type exp <: Exp
  
  trait Exp extends super.Exp {
    def print(): String
  }
  
  trait Num extends super.Num with Exp {
    override def print(): String = value.toString()
  }
}

trait PlusNegPrinter extends BasePlusNeg with BasePrinter {
  type exp <: super[BasePrinter].Exp
  
  trait Plus extends super.Plus with super[BasePrinter].Exp {
    override def print(): String = left.print() + " + " + right.print()
  }
  
  trait Neg extends super.Neg with super[BasePrinter].Exp {
    override def print(): String = "-(" + expr.print() + ")"
  }
}

object OderskyCalculator extends PlusNegPrinter {
  type exp = Exp
  
  def Num(v: Double) = new Num { value = v }
  def Neg(e: exp) = new Neg { expr = e }
  def Plus(l: exp, r: exp) = new Plus { left = l; right = r }
  
  def runTests() {
    println(Plus(Neg(Plus(Num(1), Num(2))), Num(6)).print())
  }
}