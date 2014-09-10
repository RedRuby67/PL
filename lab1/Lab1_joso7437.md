# Lab1.md

## Jonathan Song

## Problem 1

### Part (a)

	```
	1 val pi = 3.14
	2 def circumference(r: Double): Double = {
	3	val pi = 3.14159
	4 	2.0 * pi * r
	5 }
	6 def area(r: Double): Double =
	7 	pi * r * r
	
	```
	
	The pi at line 4 is bound at line 3 because it is in the scope of 
	the circumference function. The pi at line 7 is bound at 
	line 1 because it is not bound within the area function and therefore
	takes the global scope of pi at the first line.
	
### Part (b)

	```
	1 val x = 3
	2 def f(x: Int): Int =
	3 	x match {
	4 		case 0 => 0
	5 		case x => {
	6 			val y = x + 1
	7 			({
	8 				val x = y + 1
	9 				y
	10 			} * f(x - 1))
	11 		}
	12 	}
	13 val y = x + f(x)
	
	```
	
	The pi at line 3 is bound at line 2 because it is within the scope 
	of the f function. The pi at line 6 is bound at
	line 5 because it is the scope of the case match to x. 
	The pi at line 10 is bound at line 5 because it is within the scope of
	the case match to x. The pi at line 13 is bound at line 1 because
	it takes the global scope of x and is outside of the function f.

## Problem 2

	```
	1 def g(x: Int) = {
	2 	val (a, b) = (1, (x, 3))
	3 	if (x == 0) (b, 1) else (b, a + 2)
	4 }
	```
	
	Yes, the body expression of g is well typed with type
	((Int, Int), Int).
	
	(b,1) : ((Int, Int), Int)
	because b: (Int, Int) and 1: Int
	
	and
	
	(b, a + 2) : ((Int, Int), Int)
	because b: (Int, Int) and a+2: Int
