object Lab3 extends jsy.util.JsyApplication {
  import jsy.lab3.ast._
  
  /*
* CSCI 3155: Lab 3
* Jonathan Song
*
* Partner: Louis Francois
* 
* Collaborators: 
*/

  /*
* Fill in the appropriate portions above by replacing things delimited
* by '<'... '>'.
*
* Replace 'YourIdentiKey' in the object name above with your IdentiKey.
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
* code that does not compile or causes a failing assert. Simply put in a
* 'throws new UnsupportedOperationException' as needed to get something
* that compiles without error.
*/
  
  type Env = Map[String, Expr]
  val emp: Env = Map()
  def get(env: Env, x: String): Expr = env(x)
  def extend(env: Env, x: String, v: Expr): Env = {
    require(isValue(v))
    env + (x -> v)
  }
  
  def toNumber(v: Expr): Double = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => n
      case B(false) => 0
      case B(true) => 1
      case Undefined => Double.NaN
      case S(s) => try s.toDouble catch { case _: Throwable => Double.NaN }
      case Function(_, _, _) => Double.NaN
    }
  }
  
  def toBoolean(v: Expr): Boolean = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) if (n compare 0.0) == 0 || (n compare -0.0) == 0 || n.isNaN => false
      case N(_) => true
      case B(b) => b
      case Undefined => false
      case S("") => false
      case S(_) => true
      case Function(_, _, _) => true
    }
  }
  
  def toStr(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => if (n.isWhole) "%.0f" format n else n.toString
      case B(b) => b.toString
      case Undefined => "undefined"
      case S(s) => s
      case Function(_, _, _) => "function"
    }
  }

  /* Big-Step Interpreter with Dynamic Scoping */
  
  /*
* This code is a reference implementation of JavaScripty without
* strings and functions (i.e., Lab 2). You are to welcome to
* replace it with your code from Lab 2.
*/
  def eval(env: Env, e: Expr): Expr = {
    def eToN(e: Expr): Double = toNumber(eval(env, e))
    def eToB(e: Expr): Boolean = toBoolean(eval(env, e))
    def eToVal(e: Expr): Expr = eval(env, e)
    e match {
      /* Base Cases */
      case _ if isValue(e) => e
      // EvalVar
      case Var(x) => get(env, x)
      
      /* Inductive Cases */
      case Print(e1) => println(pretty(eval(env, e1))); Undefined
      
      // EvalNeg
      case Unary(Neg, e1) => 
        N(-eToN(e1))
      // EvalNot
      case Unary(Not, e1) => 
        B(!eToB(e1))
      
      // EvalPlus
      case Binary(Plus, e1, e2) => (eToVal(e1), eToVal(e2)) match {
        case (S(s1), S(s2)) => S(s1 + s2)
        case (S(s1), v2) => S(s1 + toStr(v2))
        case (v1, S(s2)) => S(toStr(v1) + s2)
        case (v1, v2) => N(toNumber(v1) + toNumber(v2))
        case _ => N(eToN(e1) + eToN(e2))
      }
      
      // EvalArith
      case Binary(Minus, e1, e2) => N(eToN(e1) - eToN(e2))
      case Binary(Times, e1, e2) => N(eToN(e1) * eToN(e2))
      case Binary(Div, e1, e2) => N(eToN(e1) / eToN(e2))
      
      // EvalEquality
      case Binary(bop @ (Eq | Ne), e1, e2) => 
        (bop: @unchecked) match {
        // added cases for Eq and Ne
          
        case Eq => (eToVal(e1), eToVal(e2)) match {
        	// need to throw error if function is an input
            case (Function(_,_,_), e2) => throw new DynamicTypeError(e)
            case (e1, Function(_,_,_)) => throw new DynamicTypeError(e)
            case (e1, e2) => 
              B(toNumber(eToVal(e1)) == toNumber(eToVal(e2)))
            case _ => 
              B(toNumber(eToVal(e1)) == toNumber(eToVal(e2)))
          }
            
         case Ne => (eToVal(e1), eToVal(e2)) match {
        	// again need to throw error if function is input
            case (Function(_,_,_), e2) => throw new DynamicTypeError(e)
            case (e1, Function(_,_,_)) => throw new DynamicTypeError(e)
            case (e1, e2) => 
              B(toNumber(eToVal(e1)) != toNumber(eToVal(e2)))
            case _ => 
              B(toNumber(eToVal(e1)) != toNumber(eToVal(e2)))
          }
        }
      
        
      // Lt
      case Binary(Lt, e1, e2) => (eToVal(e1), eToVal(e2)) match{
		case (S(v1), S(v2)) => B(v1 < v2)
		case (v1, v2) => B(toNumber(v1) < toNumber(v2))
      }
      
      // Le
      case Binary(Le, e1, e2) => (eToVal(e1), eToVal(e2)) match{
		case (S(v1), S(v2)) => B(v1 <= v2)
		case (v1, v2) => B(toNumber(v1) <= toNumber(v2))
      }
      
      // Gt
      case Binary(Gt, e1, e2) => (eToVal(e1), eToVal(e2)) match{
        case (S(v1), S(v2)) => B(v1 > v2)
		case (v1, v2) => B(toNumber(v1) > toNumber(v2))
      }
      
      // Ge
      case Binary(Ge, e1, e2) => (eToVal(e1), eToVal(e2)) match{
        case (S(v1), S(v2)) => B(v1 >= v2)
		case (v1, v2) => B(toNumber(v1) >= toNumber(v2))
      }
      
      // EvalAndTrue and EvalAndFalse
      case Binary(And, e1, e2) =>
        if (eToB(eToVal(e1))) eToVal(e2) else eToVal(e1)
      
      // EvalOrTrue and EvalOrFalse
      case Binary(Or, e1, e2) =>
        if (eToB(eToVal(e1))) eToVal(e1) else eToVal(e2)
      
      // EvalSeq
      case Binary(Seq, e1, e2) => 
        eToVal(e1); eToVal(e2)
      
      // EvalIfTrue and EvalIfFalse
      case If(e1, e2, e3) => 
        if (eToB(e1)) eToVal(e2) else eToVal(e3)
      
      // EvalConst
      case ConstDecl(x, e1, e2) => 
        eval(extend(env, x, eToVal(e1)), e2)
      
      // EvalCall
      case Call(e1, e2) => eToVal(e1) match {
        // need to check for 3 cases: function with no name, function
        // with arbitary name, error otherwise
        
        // case for function without name
        case Function(None, x, eprime) => {
          eval(extend(env, x, eToVal(e2)), eprime)
        }
        
        // case for function with name
        case Function(Some(x1), x2, eprime) => {
          eval(extend(extend(env, x1, eval(env, e1)), x2, eval(env, e2)), eprime)
        }
        // throw error otherwise
        case _ => 
          throw new DynamicTypeError(e)
      }
      
      case _ => throw new UnsupportedOperationException
    }
  }
    

  /* Small-Step Interpreter with Static Scoping */
  def substitute(e: Expr, v: Expr, x: String): Expr = {
    /* Simple helper that calls substitute on an expression with the input value v and variable name x. */
    def subst(e: Expr): Expr = substitute(e, v, x)
    
    /* Body */
    e match {
      //Simple cases
      case N(_) | B(_) | Undefined | S(_) => e
      
      // added cases
     
      case Var(y) => 
        if (x == y) v else Var(y)
      
      case ConstDecl(y, e1, e2) => 
        if (x == y) ConstDecl(y, subst(e1), e2) else ConstDecl(y, subst(e1), subst(e2))
     
      case Unary(uop, e1) => 
        Unary(uop, subst(e1))
        
      case Binary(bop, e1, e2) => 
        Binary(bop, subst(e1), subst(e2))
      
      case Call(e1, e2) => 
        Call(subst(e1), subst(e2))
      
      case If(e1, e2, e3) => 
        If(subst(e1), subst(e2), subst(e3))
      
      case Print(e1) => 
        Print(subst(e1))
      
      case Function(x1, x2, xprime) => 
        if (x1 == Some(x) | x2 == x) Function(x1, x2, xprime) else Function(x1, x2, subst(xprime))
         
       
      case _ => throw new UnsupportedOperationException
    }
  }
    
  def step(e: Expr): Expr = {
    e match {
      /* Base Cases: Do Rules */
      
      //DoPrint
      case Print(v1) if isValue(v1) => println(pretty(v1)); Undefined
      
       // ****** Your cases here
      
      // DoNeg
      case Unary(Neg, v1) if (isValue(v1)) => 
         N(-toNumber(v1)) 
         
      // DoNot
      case Unary(Not, v1) if (isValue(v1)) => 
         B(!toBoolean(v1))
      
      // DoArith
      case Binary(bop, v1, v2) if (isValue(v1) && isValue(v2)) => (bop: @unchecked, v1, v2) match {
    	 // DoPlus
    	// DoPlusString1
      	case (Plus, S(s), v2) => 
      	  S(s + toStr(v2))
      	// DoPlusString2
      	case (Plus, v1, S(s)) => 
      	  S(toStr(v1) + s)
      	// DoPlusNumber
        case (Plus, v1, v2) => 
          N(toNumber(v1) + toNumber(v2))
          
        case (Times, v1, v2) =>
          N(toNumber(v1) * toNumber(v2))
        case (Div, v1, v2) =>
          N(toNumber(v1) / toNumber(v2))
        case (Minus, v1, v2) =>
          N(toNumber(v1) - toNumber(v2))
          
        //DoEquality
        // throw error if pass in function
        case ((Eq|Ne), Function(_,_,_), v2) =>
          throw new DynamicTypeError(e)
        case ((Eq|Ne), v1, Function(_,_,_)) =>
          throw new DynamicTypeError(e)
         
         case (Eq, v1, v2) => B(toBoolean(v1) == toBoolean(v2))	
         case (Ne, v1, v2) => B(toBoolean(v1) != toBoolean(v2))
        
         // DoInequalityString
         case (Lt, S(v1), S(v2)) =>
           B(v1 < v2)
         case (Le, S(v1), S(v2)) =>
           B(v1 <= v2)
         case (Gt, S(v1), S(v2)) =>
           B(v1 > v2)
         case (Ge, S(v1), S(v2)) =>
           B(v1 >= v2)
          
        //DoInequalityNumber
        case (Lt, v1, v2) => 
          B(toNumber(v1) < toNumber(v2))
        case (Le, v1, v2) => 
          B(toNumber(v1) <= toNumber(v2))
        case (Gt, v1, v2) => 
          B(toNumber(v1) > toNumber(v2))
        case (Ge, v1, v2) => 
          B(toNumber(v1) >= toNumber(v2))
       
        //DoAndTrue and DoAndFalse
        case (And, v1, v2) => 
          if(toBoolean(v1)) v2 else v1
      
       //DoOrTrue and DoOrFalse
       case (Or, v1, v2) => 
         if(toBoolean(v1)) v1 else v2
         
      }
      
      // DoAnd and DoOr 
      //DoAndTrue & DoAndFalse
       case Binary(And, v1, e2) if (isValue(v1)) => 
         if(toBoolean(v1)) e2 else v1
      
       //DoOrTrue and DoOrFalse
        case Binary(Or, v1, e2) if (isValue(v1)) => 
          if(toBoolean(v1)) v1 else e2
     
      
      //DoIfTrue and DoIfFalse 
      case If(v1, e2, e3) if(isValue(v1)) => 
        if(toBoolean(v1)) e2 else e3
      
      //DoConst 
      case ConstDecl(x, v1, e2) if(isValue(v1)) => 
        substitute(e2, v1, x)
      
        
      //DoCall
      case Call(v1, v2) if ((isValue(v1)) && (isValue(v2))) =>
        v1 match {
          case Function(None, x, e1) =>
            substitute(e1, v2, x)
          case Function(Some(x1), x2, e1) =>
            substitute(substitute(e1, v1, x1), v2, x2)
          case _ =>
            throw new DynamicTypeError(e)
        }
        
        
      //DoSeq
      case Binary(Seq, v1, e2) if (isValue(v1)) => 
        e2
      
        
      /* Inductive Cases: Search Rules */

      // SearchPrint
      case Print(e1) => Print(step(e1))
      
      // ****** Your cases here
      
      //SearchUnary
      case Unary(uop, e1) => 
        val e1prime = step(e1)
        Unary(uop, e1prime)
       
      // SearchBinaryArith2 and SearchEquality2
      case Binary(bop, v1, e2) if (isValue(v1)) =>
        val e2prime = step(e2)
        Binary(bop, v1, e2prime)
        
     //SearchBinary1
     case Binary(bop, e1, e2) =>
       val e1prime = step(e1)
       Binary(bop, e1prime, e2)
        
      // TypeErrorEquality
      case Binary(Eq | Ne, v1, e2) if (isValue(v1)) => v1 match {
        case Function(_,_,_) =>
          throw new DynamicTypeError(e)
      }
    
      //TypeErrorEquality1
      case Binary(bop, e1, Function(_,_,_)) =>
        throw new DynamicTypeError(e)
      
      //SearchIf
      case If(e1, e2, e3) => 
        val e1prime = step(e1)
        if(toBoolean(e1prime)) e2 else e3
      
      //SearchConst
      case ConstDecl(x, e1, e2) => 
        val e1prime = step(e1)
        ConstDecl(x, e1prime, e2)
        
      //SearchCall 
      case Call(e1, e2) => (e1, e2) match {
       
       case (e1@Function(_,_,_), e2) =>
         val e2prime = step(e2)
         Call(e1, e2prime)
         
       //TypeErrorCall 
       case (v1, e2) if (isValue(v1))=> 
         throw new DynamicTypeError(e) 
     
        //SearchCall1
       case (e1, e2) => 
         val e1prime = step(e1)
         Call(e1prime, e2)
         
      }
          
      /* Cases that should never match. Your cases above should ensure this. */
      case Var(_) => throw new AssertionError("Gremlins: internal error, not closed expression.")
      case N(_) | B(_) | Undefined | S(_) | Function(_, _, _) => throw new AssertionError("Gremlins: internal error, step should not be called on values.");
    }
  }
  

  /* External Interfaces */
  
  this.debug = true // comment this out or set to false if you don't want print debugging information
  
  // Interface to run your big-step interpreter starting from an empty environment and print out
  // the test input if debugging.
  def evaluate(e: Expr): Expr = {
    require(closed(e))
    if (debug) {
      println("------------------------------------------------------------")
      println("Evaluating with eval ...")
      println("\nExpression:\n " + e)
    }
    val v = eval(emp, e)
    if (debug) {
      println("Value: " + v)
    }
    v
  }
  
  // Convenience to pass in a jsy expression as a string.
  def evaluate(s: String): Expr = evaluate(jsy.lab3.Parser.parse(s))
   
  // Interface to run your small-step interpreter and print out the steps of evaluation if debugging.
  def iterateStep(e: Expr): Expr = {
    require(closed(e))
    def loop(e: Expr, n: Int): Expr = {
      if (debug) { println("Step %s: %s".format(n, e)) }
      if (isValue(e)) e else loop(step(e), n + 1)
    }
    if (debug) {
      println("------------------------------------------------------------")
      println("Evaluating with step ...")
    }
    val v = loop(e, 0)
    if (debug) {
      println("Value: " + v)
    }
    v
  }

  // Convenience to pass in a jsy expression as a string.
  def iterateStep(s: String): Expr = iterateStep(jsy.lab3.Parser.parse(s))
  
  // Interface for main
  def processFile(file: java.io.File) {
    if (debug) {
      println("============================================================")
      println("File: " + file.getName)
      println("Parsing ...")
    }
    
    val expr =
      handle(None: Option[Expr]) {Some{
        jsy.lab3.Parser.parseFile(file)
      }} getOrElse {
        return
      }
    
    handle() {
      val v = evaluate(expr)
      println(pretty(v))
    }
    
    handle() {
      val v1 = iterateStep(expr)
      println(pretty(v1))
    }
  }
    
}
