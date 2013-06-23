.PHONY=run clean

all: expression run

expression: Expression.scala
	scalac Expression.scala Calculator.scala

run: expression
	scala se464.calculator.Calculator

clean:
	rm *.class
