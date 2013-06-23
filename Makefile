.PHONY=run clean

all: expression run

expression: Expression.scala
	scalac Expression.scala

run: expression
	scala Calculator

clean:
	rm *.class
