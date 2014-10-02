# Lab2.md

## Jonathan Song

## Problem 1

### Part (a)

(A1 ε AObjects,  A2 ε AObjects)/(A1 & A2 ε AObjects)

(V ε VObjects)/(V ε AObjects)

/(a ε VObjects)

/(b ε VObjects)

	
### Part (b)

	   A				
	/  |  \
   A   &   A
 / | \     |
A  &  A    V   	
|	  |    |
b     a	   b

	   A
	/  |  \
   A   &   A
   |     / | \
   V    A  &  A
   |    |     |
   b    a     b
	
<p>
Both of these trees yield the sentence b & a & b, showing that the 
grammar is ambiguous. 
</p>
	
### Part (c) 

<p>
This language yields strings with one or more a's, one or more b's, and
one or more c's. 
</p>	

### Part (d)

<p>
1) S => AaBb => baBb => baab
2) This sentence is cannot be described by the grammar.
3) This sentence is cannot be described by the grammar.
4) S => AaBb => AbaBb => bbaBb => bbaab
</p>

### Part (e)

<p>
1) 				S
			  /	| \ \
             a  S c  B
                |    |
                b    d

2) This sentence is cannot be described by the grammar.
3) This sentence is cannot be described by the grammar.
4) This sentence is cannot be described by the grammar.
5)				S
			  / | \ \
			 a  S  c B
				|    |
				c    c

</p>
	
## Problem 2

### Part (a)

<p>
i. The expressions generated by these grammars contain one or more
operands seprated by operators.

ii. These grammers may generate different parse trees and evaluations,
but they end up generating the same expressions. The main difference 
here is that the first grammer is left-associative while the second
grammar is right-associative.

</p>


### Part (b)
<p>
A scala expression that demonstrates that '-' has a higher precedence
than '<<' could is: 5 << 7-3 << 2. When run on the interpreter, this 
evaluates to 192. This expression and evaluation demonstrates that 
'-' has higher precedence, as the expression evaluates to 
5 << (7-3) << 2, which is 5 << 4 << 2 and also evaluates to 192. It also
disproves that '<<' has higher precedence, because the expression
(5 << 7) - (3 << 2) evaluates to 628, which is not the same as the 
original expression evaluation. 
</p>


### Part (c)
<p>

Float ::= Fractions | Exponents

Fractions ::= Sign | Sign n 

Exponent ::= E Sign | ε

Sign ::= n | -n

D ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 

</p>