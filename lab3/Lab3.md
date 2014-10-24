# Lab3.md

## Jonathan Song

## Problem 1

### Part (a)

```
1 const x = 10
2 const foo = function(y) { return x};
3 const bar = function(x) {return foo(3)};
4 bar(7)
```

<p>
This function will behave differently under dynamic and static scoping.
Under static scoping, the variable x will remain bound to the initial value
of 10 at line 1 and will return 10 once bar(7) is called. The environment
within static scoping will map x to 10 and y to 3 an will return 10
without changingn the variable mapping. However, under dynamic scoping, 
the value bound to x will change from 10 to 7 and will print out 7 
instead in this case. This can be seen in the envoronment of dynamic
scoping that will map x to 10 and y to 3, but will have another assignment
of x to 7 that will override the original assignment of x to 10.
</p>

## Problem 2

### Part (c)
<p>
The evaluation order is deterministic and evaluates from left
to right.
</p>

## Problem 3
<p>
The evaluation order for this program in left to right, as is seen by
the judgment forms, which do not allow for evaluation of the right side
of an equation until the left side has been evaluated to a value first.
The evaluation order can be reversed by basically reversing e1 and e2 in 
the rules in such a way that it forces the program to first evaluate e2 to
a value before e1 can be evaluated.
</p>


## Problem 4

### Part (a)
<p>
Short circuit evaluation is useful because it allows us to avoid
evaluating the right side of an equation in cases when the value of the 
left side coupled with the operation makes the result obvious without 
us needing to evaluate the right side to a value. This is particularily 
useful in cases when e2 might be something massive or complicated and
may take several steps to evaluate to a value and would end up wasting 
time. For example, in the case of an expression such as 0 * e2, because 
the left side is zero and the operation is multiplication, it is already 
known that this expression evaluates to zero. 
</p>

### Part (b)
<p>
The rules for e1 && e2 seem to indicate that the small step 
operation semantics short circuit in cases when e1, the left side
of the equation is false. It demonstrates that when e1 is false, then
the expression returns false regardless of the value of e2.
</p>

