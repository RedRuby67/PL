object Lab1 extends jsy.util.JsyApplication {
  import jsy.lab1.ast._
  import jsy.lab1.Parser
  
  /*
   * CSCI 3155: Lab 1
   * Jonathan Song
   * 
   * Partner: Alex Campbell
   * Collaborators: <Any Collaborators>
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   * 
   * Replace the 'throw new UnsupportedOperationException' expression with
   * your code in each function.
   * 
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   * 
   * Your lab will not be graded if it does not compile.
   * 
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert.  Simply put in a
   * 'throws new UnsupportedOperationException' as needed to get something
   * that compiles without error.
   */
  
  /*
   * Example with a Unit Test
   * 
   * A convenient, quick-and-dirty way to experiment, especially with small code
   * fragments, is to use the interactive Scala interpreter.
   * 
   * To run a selection in the interpreter in Eclipse, highlight the code of interest
   * and type Ctrl+Shift+X (on Windows) or Cmd+Shift+X (on Mac).
   * 
   * Highlight the next few lines below to try it out.  The assertion passes, so
   * it appears that nothing happens.  You can uncomment the "bad test specification"
   * and see that a failed assert throws an exception.
   * 
   * You can try calling 'plus' with some arguments, for example, plus(1,2).  You
   * should get a result something like 'res0: Int = 3'.
   * 
   * As an alternative, the testPlus2 function takes an argument that has the form
   * of a plus function, so we can try it with different implementations.  For example,
   * uncomment the "testPlus2(badplus)" line, and you will see an assertion failure.
   * 
   * Our convention is that these "test" functions are testing code that are not part
   * of the "production" code.
   * 
   * While writing such testing snippets are convenient, it is not ideal.  For example,
   * the 'testPlus1()' call is run whenever this object is loaded, so in practice,
   * it should probably be deleted for "release".  A more robust way to maintain
   * unit tests is in a separate file.  For us, we use the convention of writing
   * tests in a file called LabXSpec.scala (i.e., Lab1Spec.scala for Lab 1).
   */
  
  def plus(x: Int, y: Int): Int = x + y
  def testPlus1() {
    assert(plus(1,1) == 2)
    //assert(plus(1,1) == 3) // bad test specification
  }
  testPlus1()

  def badplus(x: Int, y: Int): Int = x - y
  def testPlus2(plus: (Int, Int) => Int) {
    assert(plus(1,1) == 2)
  }
  //testPlus2(badplus)

  /* Exercises */

  // run tests with src/test/scala / default package / Lab1Spec.scala
  // sbt "project lab1-grader" run, but not this time

  def abs(n: Double): Double = 
    // absolute value
    // if n is above 0.0, return n, else it is negative
    if (n >= 0) n else (n * -1.0)

  def xor(a: Boolean, b: Boolean): Boolean =  
  	// xor will return true if a and b are not the same and one of them is true
    // we only have the two variables, so they should only be false when they 
    // equal each other and should be true otherwise
  	if (a == b) false else true
  
  def repeat(s: String, n: Int): String = 
  	// will return string with n copies of string s concatenated together
    // no spaces
  	// string s * n
   	if (n < 0) throw new IllegalArgumentException
   	else if (n == 0) ""
   	else s + repeat(s,n-1)
   	
  
  def sqrtStep(c: Double, xn: Double): Double =  
    // using recursive equation for sequence of approximations
    // xn+1 = xn - (xn^2 - c)/(2 * xn)
    xn - (xn * xn - c)/(2 * xn)
    
  def sqrtN(c: Double, x0: Double, n: Int): Double = {
   	// need to make sure n is a positive number
    require ( n >= 0)
    n match {
      // when n is 0, evaluate to initial guess, x0
      case 0 => x0
      // using wild card _ for nonzero 
      // need to do n recursions of sqrtStep
      case _ => sqrtN(c, sqrtStep(c,x0) ,n-1)
    }
    
   	}
  
  def sqrtErr(c: Double, x0: Double, epsilon: Double): Double = {
    // computes approximations xn until error is within epsilon
   	// abs(xn*xn -c) < epsilon
    require(epsilon > 0)
    if (abs(x0*x0 - c) <= epsilon) x0
    else sqrtErr(c, sqrtStep(c,x0), epsilon)
  }
  
  def sqrt(c: Double): Double = {
    require(c >= 0)
    if (c == 0) 0 else sqrtErr(c, 1.0, 0.0001)
  }
  
  /* Search Tree */
  
  sealed abstract class SearchTree
  case object Empty extends SearchTree
  case class Node(l: SearchTree, d: Int, r: SearchTree) extends SearchTree
  
  // checks if instance is a valid BST
  def repOk(t: SearchTree): Boolean = {
    // t is passed in as parameter as a SearchTree
    // case match with t, the search tree, which is empty or a node
    def check(t: SearchTree, min: Int, max: Int): Boolean = t match {
      // first check case where search tree is empty
      // trivial case
      case Empty => true
      // next need to check case where search tree is a node with left child l, data value d, and
      // right child r
      case Node(l, d, r) => 
        if (d >= min && d <= max) (check(l, min, d) && check(r, d, max))
        else false
        // need to check tree for l < d <= r 
    }
    check(t, Int.MinValue, Int.MaxValue)
  }
  
  def insert(t: SearchTree, n: Int): SearchTree = {
    // use cases concerning the search tree
    t match {
      // need to check for empty node case
      // if empty, just add n as the integer data
      case Empty => Node(Empty, n, Empty)
      // check for search tree node
      case Node(l, d, r) => {
        // given the value to insert is same as data of node,
        // we can just put it in right away

        // if the value to insert is less than the data of node,
        // we have to insert it at the left because lower values
        // go on the left side of the tree
        // recurse on left tree
        if (n < d)
          Node(insert(l, n), d, r)
        // otherwise value is greater than data of node and
        // should go on right of node because data on the
        // right of a node is greater
        // recurse of right tree
        else
          Node(l, d, insert(r, n))
      }
    }
  }
  
  def deleteMin(t: SearchTree): (SearchTree, Int) = {
    // returns updated tree and data value of deleted node
    // t must not be an empty tree
    require(t != Empty)
    (t: @unchecked) match {
      // checking case for empty left subtree
      case Node(Empty, d, r) => (r, d)
      // case for nonempty left subtree
      case Node(l, d, r) =>
        // recursive call to deleteMin(l) with left subtree as input parameter
        // going down tree on left side will most likely get you to the
        // minimum value
        val (l1, min) = deleteMin(l)
        // will allow recursive call to continually travel down left subtree presumably until it hits
        // the smallest data element when there is no left subtree and it hits the empty left 
        // subtree case
        (Node(l1,d, r), min)
    }
  }
 
  def delete(t: SearchTree, n: Int): SearchTree = 
    // removes first node with value n
    // returns new SearchTree without node of data value n
    {
    // using t as search tree for cases
     t match {
      // if t is empty, nothing to do 
      case Empty => t
      // check case for data value without children
      case Node(Empty, d, Empty) => 
        if (d == n) Empty else t
      // check cases for recursion if node only has one child
      // will have to perform recursion of present child
      // need to recurse through tree in valid direction
      // so we don't hit a dead end
      case Node(Empty, d, r) => 
        if (d == n) r else Node(Empty, d, delete(r, n))
      case Node(l, d, Empty) => 
        if (d == n) l else Node(delete(l, n), d, Empty)
      // now check cases if node has both children
      case Node(l, d, r) => {
        // recursive case when value to delete is found
        if (n == d) {
          val (r1, rightmin) = deleteMin(r)
          Node(l, rightmin, r1)
        //recursive case to move down tree if n < d
        // proceed down left child
        } else if (n < d) {
          Node(delete(l, n), d, r)
        }
        // recursive case to move down tree for remaining case
        // n > d
        // proceed down right child
        else {
          Node(l, d, delete(r, n))
        }
      }
    }
  }
  
  /* JavaScripty */
  
  def eval(e: Expr): Double = e match {
    // basically building a calculator
    case N(n) => return n
    case Unary(Neg, e1) => -1 * eval(e1)
    case Binary (Plus, e1, e2) => eval(e1) + eval(e2)
    case Binary (Minus, e1, e2) => eval(e1) - eval(e2)
    case Binary (Times, e1, e2) => eval(e1) * eval(e2)
    case Binary (Div, e1, e2) => {
      if (e2 == 0) throw new ArithmeticException
      else eval(e1) / eval(e2)
    }
  }
  
 // Interface to run your interpreter from a string.  This is convenient
 // for unit testing.
 def eval(s: String): Double = eval(Parser.parse(s))



 /* Interface to run your interpreter from the command-line.  You can ignore the code below. */ 
  
 def processFile(file: java.io.File) {
    if (debug) { println("Parsing ...") }
    
    val expr = Parser.parseFile(file)
    
    if (debug) {
      println("\nExpression AST:\n  " + expr)
      println("------------------------------------------------------------")
    }
    
    if (debug) { println("Evaluating ...") }
    
    val v = eval(expr)
    
    println(v)
  }

}
