object Lab4 extends jsy.util.JsyApplication {
  import jsy.lab4.ast._
  import jsy.lab4.Parser
  
  /*
   * CSCI 3155: Lab 4
   * Jonathan Song
   * 
   * Partner: Louis Francois
   * Collaborators: <Any Collaborators>
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
   * code that does not compile or causes a failing assert.  Simply put in a
   * 'throws new UnsupportedOperationException' as needed to get something
   * that compiles without error.
   */
  
  /* Collections and Higher-Order Functions */
  
  /* Lists */
  
  // eliminates consecutive duplicates of list elements
  // do not change order of elements
  // implement with direct recursion on compressRec()
  def compressRec[A](l: List[A]): List[A] = l match {
    case Nil | _ :: Nil => l
    case h1 :: (t1 @ (h2 :: _)) => 
      // no duplicates, so recurse on t1 if h1 == h2
      // otherwise concatenate h1 and compressRec(tl)
      if (h1 == h2) compressRec(t1) else h1 :: compressRec(t1)
  }
  
  // use foldRight from library
  def compressFold[A](l: List[A]): List[A] = l.foldRight(Nil: List[A]) {
    (h, acc) => acc match {
      case Nil => h :: acc
      // exclude h if h == h1, otherwise concatenate h :: acc
      case h1 :: h2 => if (h == h1) acc else h :: acc
    }
  }
  
  // finds the first element in l and returns Some(a) for value a
  // replaces that element with a and leaves l same elsewhere
  def mapFirst[A](f: A => Option[A])(l: List[A]): List[A] = l match {
    case Nil => l
    case h :: t => f(h) match {
      // replaces first element for some a
      case Some(a) => a :: t
      // otherwise recurse through elements, taking in the same
      // option f and changing the list to the tail
      case None => h :: mapFirst(f)(t)
    }
  }
  
  /* Search Trees */
  
  sealed abstract class Tree {
    def insert(n: Int): Tree = this match {
      case Empty => Node(Empty, n, Empty)
      case Node(l, d, r) => if (n < d) Node(l insert n, d, r) else Node(l, d, r insert n)
    } 
    
    // performs in-order traversal of input tree
    def foldLeft[A](z: A)(f: (A, Int) => A): A = {
      def loop(acc: A, t: Tree): A = t match {
        case Empty => acc
        // recurse and accumulate on left side of input tree
        case Node(l, d, r) => f(loop(loop(acc, l), r), d)
      }
      loop(z, this)
    }
    
    def pretty: String = {
      def p(acc: String, t: Tree, indent: Int): String = t match {
        case Empty => acc
        case Node(l, d, r) =>
          val spacer = " " * indent
          p("%s%d%n".format(spacer, d) + p(acc, l, indent + 2), r, indent + 2)
      } 
      p("", this, 0)
    }
  }
  case object Empty extends Tree
  case class Node(l: Tree, d: Int, r: Tree) extends Tree
  
  def treeFromList(l: List[Int]): Tree =
    l.foldLeft(Empty: Tree){ (acc, i) => acc insert i }
  
  def sum(t: Tree): Int = t.foldLeft(0){ (acc, d) => acc + d }
  
  // client of foldLeft and checks that data values of tree t are in order
  def strictlyOrdered(t: Tree): Boolean = {
    val (b, _) = t.foldLeft(true, None: Option[Int]) {
	// foldLeft acts like a for loop that goes from left to right,
	// so in this case, the accumulator must be one the left side, and
	// the data value is one the right
      (acc, d) => (acc, d) match {
        case ((false, Some(a)), _) => (false, Some(a))
        case ((true, None), c) => (true, Some(c))
        // we have to return true when a > c, false otherwise
        case ((b, Some(a)), c) => if (c < a) (true, Some(c)) 
        						  else (false, Some(a))
      }
    }
    b
  }
  

  /* Type Inference */
  
  // A helper function to check whether a jsy type has a function type in it.
  // While this is completely given, this function is worth studying to see
  // how library functions are used.
  def hasFunctionTyp(t: Typ): Boolean = t match {
    case TFunction(_, _) => true
    case TObj(fields) if (fields exists { case (_, t) => hasFunctionTyp(t) }) => true
    case _ => false
  }
  
  def typeInfer(env: Map[String,Typ], e: Expr): Typ = {
    // Some shortcuts for convenience
    def typ(e1: Expr) = typeInfer(env, e1)
    def err[T](tgot: Typ, e1: Expr): T = throw StaticTypeError(tgot, e1, e)

    e match {
      case Print(e1) => typ(e1); TUndefined
      case N(_) => TNumber
      case B(_) => TBool
      case Undefined => TUndefined
      case S(_) => TString
      
      // TypeVar
      case Var(x) => env(x)
      case ConstDecl(x, e1, e2) => typeInfer(env + (x -> typ(e1)), e2)
      
      // TypeNeg
      case Unary(Neg, e1) => typ(e1) match {
        case TNumber => TNumber
        case tgot => err(tgot, e1)
      }
      
      // TypeNot
      case Unary(Not, e1) => typ(e1) match {
        case TBool => TBool
        case tgot => err(tgot, e1)
      }
      
      // TypePlus
      case Binary(Plus, e1, e2) => (typ(e1), typ(e2)) match {
        // TypePlusNumber
        case (TNumber, TNumber) => TNumber
        // TypePlusString
        case (TString, TString) => TString
        case _ => err(typ(e1), e1)
      }
      // TypeArith
      case Binary(Minus|Times|Div, e1, e2) => (typ(e1), typ(e2)) match {
        case (TNumber, TNumber) => TNumber
        case _ => err(typ(e1), e1)
      }
      
      // TypeEquality
      case Binary((Eq|Ne), e1, e2) => (typ(e1), typ(e2)) match {
        // t has no function types  
      	case (TNumber, TNumber) => TBool
      	case (TString, TString) => TBool
      	case (TBool, TBool) => TBool
      	case (TFunction(a, b), _) => err(TFunction(a, b), e1)
      	case (_, TFunction(a, b)) => err(TFunction(a, b), e1)
      	case _ => err(typ(e1), e1)    							
      }
        
        // TypeInequality
      case Binary(Lt|Le|Gt|Ge, e1, e2) => (typ(e1), typ(e2)) match {
        // TypeInequalityNumber
      	case (TNumber, TNumber) => TBool
        // TypeInequalityString
      	case (TString, TString) => TBool
      	case _ => err(typ(e1), e1)
      }
      
      // TypeAndOr
      case Binary(And|Or, e1, e2) => (typ(e1), typ(e2)) match {
        case (TBool, TBool) => TBool
        case _ => err(typ(e1), e1)
      }
      // TypeSeq
      case Binary(Seq, e1, e2) =>
        typ(e2)
        
      // TypeIf
      case If(e1, e2, e3) => (typ(e1), typ(e2), typ(e3)) match {
        case (TBool, e2got, e3got) if (e2got == e3got) => e2got
      	case _ => err(typ(e1), e1)
      }
      
      // TypeFunction
      case Function(p, params, tann, e1) => {
        // Bind to env1 an environment that extends env with an appropriate binding if
        // the function is potentially recursive.
        val env1 = (p, tann) match {
          case (Some(f), Some(tret)) =>
            val tprime = TFunction(params, tret)
            env + (f -> tprime)
          case (None, _) => env
          case _ => err(TUndefined, e1)
        }
        
        // Bind to env2 an environment that extends env1 with bindings for params.
        // use foldleft takes in env1 with accumulator 
        // maps x1 -> t1 .... xn -> tn
        val env2 = params.foldLeft(env1) {
          case(acc, (xn, tn)) => acc + (xn -> tn)
        }
        
        tann match {
          case None => TFunction(params, typeInfer(env2, e1))
          case Some(tret) => TFunction(params, tret)
        }
      }
      
      // TypeCall
      case Call(e1, args) => typ(e1) match {
        case TFunction(params, tret) if (params.length == args.length) => {
          (params, args).zipped.foreach {
            // need to make sure type of parameter arguments matches type of return
            case ((x, t), ret)  => if (t == typ(ret)) tret else err(t, ret)
          };
          tret
        }
        case tgot => err(tgot, e1)
      }
      
      // TypeObject
      case Obj(fields) =>
        TObj(fields.mapValues((exp) => typ(exp)))
        
      // TypeGetField
      case GetField(e1, f) => typ(e1) match {
        case TObj(map) => map.get(f) match {
          case Some(f) => f
          case _ => err(typ(e1), e1)
        }
        case _ => err(typ(e1), e1)
      }
    }
  }
  
  
  /* Small-Step Interpreter */
  
  def inequalityVal(bop: Bop, v1: Expr, v2: Expr): Boolean = {
    require(bop == Lt || bop == Le || bop == Gt || bop == Ge)
    ((v1, v2): @unchecked) match {
      case (S(s1), S(s2)) =>
        (bop: @unchecked) match {
          case Lt => s1 < s2
          case Le => s1 <= s2
          case Gt => s1 > s2
          case Ge => s1 >= s2
        }
      case (N(n1), N(n2)) =>
        (bop: @unchecked) match {
          case Lt => n1 < n2
          case Le => n1 <= n2
          case Gt => n1 > n2
          case Ge => n1 >= n2
        }
    }
  }
  
   def substitute(e: Expr, v: Expr, x: String): Expr = {
    require(isValue(v))
    
    def subst(e: Expr): Expr = substitute(e, v, x)
    
    e match {
      case N(_) | B(_) | Undefined | S(_) => e
      case Print(e1) => Print(subst(e1))
      case Unary(uop, e1) => Unary(uop, subst(e1))
      case Binary(bop, e1, e2) => Binary(bop, subst(e1), subst(e2))
      case If(e1, e2, e3) => If(subst(e1), subst(e2), subst(e3))
      case Var(y) => if (x == y) v else e
      case ConstDecl(y, e1, e2) => ConstDecl(y, subst(e1), if (x == y) e2 else subst(e2))
      
      // added cases    
      case f1 @ Function(p, params, tann, e1) => {
        if (Some(x) == p || params.exists((t1: (String, Typ)) => t1._1 == x)) {
          f1
        } else {
          Function(p, params, tann, subst(e1))
        }
      }
	
      case Call(e1, args) => 
        Call(subst(e1), args map subst)
      
      case Obj(fields) => 
        Obj(fields.mapValues(v => subst(v)))
      
      case GetField(e1, f) => 
        GetField(subst(e1), f)
    }
  }
  
   
  def step(e: Expr): Expr = {
    require(!isValue(e))
    
    def stepIfNotValue(e: Expr): Option[Expr] = if (isValue(e)) None else Some(step(e))
    
    e match {
      /* Base Cases: Do Rules */
      case Print(v1) if isValue(v1) => println(pretty(v1)); Undefined
      case Unary(Neg, N(n1)) => N(- n1)
      case Unary(Not, B(b1)) => B(! b1)
      case Binary(Seq, v1, e2) if isValue(v1) => e2
      case Binary(Plus, S(s1), S(s2)) => S(s1 + s2)
      case Binary(Plus, N(n1), N(n2)) => N(n1 + n2)
      
      // need DoArith
      case Binary(Minus, N(n1), N(n2)) => N(n1 - n2)
      case Binary(Times, N(n1), N(n2)) => N(n1 * n2)
      case Binary(Div, N(n1), N(n2)) => N(n1 / n2)
     
      case Binary(bop @ (Lt|Le|Gt|Ge), v1, v2) if isValue(v1) && isValue(v2) => B(inequalityVal(bop, v1, v2))
      
      case Binary(Eq, v1, v2) if isValue(v1) && isValue(v2) => B(v1 == v2)
      case Binary(Ne, v1, v2) if isValue(v1) && isValue(v2) => B(v1 != v2)
      case Binary(And, B(b1), e2) => if (b1) e2 else B(false)
      case Binary(Or, B(b1), e2) => if (b1) B(true) else e2
      case ConstDecl(x, v1, e2) if isValue(v1) => substitute(e2, v1, x)
      
      // need DoIf
      // DoIfTrue & DoIfFalse
      case If(B(b1), e2, e3) =>
        if (b1) e2 else e3
      
      // DoCall
      case Call(v1, args) if isValue(v1) && (args forall isValue) =>
        v1 match {
          case Function(p, params, t, e1) => {
            //Zipped works by appending the first list with the second
            //A list of tupples and a list become List(Tuple, Val)
            val e1p = (params, args).zipped.foldRight(e1) {
              (vars: ((String, Typ), Expr), acc: Expr) => (vars, acc) match {
                // e[v1/x1]...e[vn/xn]
              	case (((x, t), v1), e1) => substitute(e1, v1, x)
              }
            }
            p match {
              case None => e1p
              case Some(x1) => substitute(e1p, Function(Some(x1), params, t, e1), x1)
            }
          }    
          
          case _ => throw new StuckError(e)
        }
        
      // need DoGetField && SearchGetField
      case GetField(v1, f) => v1 match {
       	case Obj(map) => map.get(f) match {
       		case Some(v1) if (isValue(v1)) => v1 
       		case Some(v1) => step(v1)
       		case _ => throw new StuckError(e)
        	}  	
        case _ => throw new StuckError(e)
      } 
        
             
      /* Inductive Cases: Search Rules */
      case Print(e1) => Print(step(e1))
      case Unary(uop, e1) => Unary(uop, step(e1))
      case Binary(bop, v1, e2) if isValue(v1) => Binary(bop, v1, step(e2))
      case Binary(bop, e1, e2) => Binary(bop, step(e1), e2)
      case If(e1, e2, e3) => If(step(e1), e2, e3)
      case ConstDecl(x, e1, e2) => ConstDecl(x, step(e1), e2)
      
    
      // need SearchCall
      case Call(v1, args) => v1 match {
        
        case Function(None, params, tann, e1) => {
          Call(Function(None, params, tann, e1), args.foldLeft(List(): List[Expr]){
            // use :+(elem: A): Stream[A]
            // A copy of this stream with an element appended rather than prepended
            // colon on collector's side
            (acc: List[Expr], e1: Expr) => 
              if (isValue(e1)) acc :+ e1 else acc :+ step(e1)
          })
        }
        
        case Function(Some(f), params, tann, e1) => {
          Call(Function(Some(f), params, tann, e1), args.foldLeft(List(): List[Expr]){
            (acc: List[Expr], e1: Expr) => 
              if (isValue(e1)) acc :+ e1 else acc :+ step(e1)
          })
        }
        
        case e1 => Call(step(e1), args)
      }
      
      // need SearchObject
      case Obj(map) => Obj(map.foldLeft(Map(): Map[String, Expr]) {
            (acc, x) => x match {
              case (f1, e1) if (isValue(e1)) => acc + (f1 -> e1)
              // {fi:ei} -> {fi:ei`}
              case (f1, e1) => acc + (f1 -> step(e1))
          }
      })
     
      
      /* Everything else is a stuck error. Should not happen if e is well-typed. */
      case _ => throw StuckError(e)
    }
  }
  
  
  /* External Interfaces */
  
  this.debug = true // comment this out or set to false if you don't want print debugging information
  
  def inferType(e: Expr): Typ = {
    if (debug) {
      println("------------------------------------------------------------")
      println("Type checking: %s ...".format(e))
    } 
    val t = typeInfer(Map.empty, e)
    if (debug) {
      println("Type: " + pretty(t))
    }
    t
  }
  
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
  def iterateStep(s: String): Expr = iterateStep(Parser.parse(s))
  
  // Interface for main
  def processFile(file: java.io.File) {
    if (debug) {
      println("============================================================")
      println("File: " + file.getName)
      println("Parsing ...")
    }
    
    val expr =
      handle(None: Option[Expr]) {Some{
        Parser.parseFile(file)
      }} getOrElse {
        return
      }
    
    handle() {
      val t = inferType(expr)
    }
    
    handle() {
      val v1 = iterateStep(expr)
      println(pretty(v1))
    }
  }

}
