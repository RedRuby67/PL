object Lab5 extends jsy.util.JsyApplication {
  import jsy.lab5.ast._
  import jsy.lab5._
  
  /*
   * CSCI 3155: Lab 5
   * Jonathan Song
   * 
   * Partner: Gabriella Mendoza
   * Collaborators: Louis Francois
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
  
  /*** Helper: mapFirst to DoWith ***/
  
  // Just like mapFirst from Lab 4 but uses a callback f that returns a DoWith in the Some case.
  // finds the first element in l and returns Some(a) for value a
  // replaces that element with a and leaves l same elsewhere
  def mapFirstWith[W,A](f: A => Option[DoWith[W,A]])(l: List[A]): DoWith[W,List[A]] = l match {
  	case Nil => doreturn(l)
    case h :: t => f(h) match {
      // if none match, recurse and use cons operator to concatenate head to List[A], l
      case None => mapFirstWith(f)(t).map((l:List[A]) => (h :: l))
      // if match, use map to cons A to tail
      case Some(withhp) => withhp.map((a:A) => (a :: t))
    }
  }
 
  
  /*** Casting ***/
  
  // specifies when type t1 can be casted to type t2
  // implements judgement form t1 -> t2
  def castOk(t1: Typ, t2: Typ): Boolean = (t1, t2) match {
    case (TNull, TObj(_)) => true
    case (_, _) if (t1 == t2) => true
    // need to recurse through each field of fields1's fields
    // and test for compatability
    case (TObj(fields1), TObj(fields2)) => fields1.forall {
      // if b is None, return true
      case(a, b) if (b == None) => true
      // is b Some, check a
      case(a, b) => fields2.get(a) match {
    	// if a is None, return true
        case None => true
        // recurse otherwise
        case Some(c) => castOk(b, c)
      }
    }
    case (TInterface(tvar, t1p), _) => 
      castOk(typSubstitute(t1p, t1, tvar), t2)
    case (_, TInterface(tvar, t2p)) => 
      castOk(t1, typSubstitute(t2p, t2, tvar))
    case _ => false
  }
  
  /*** Type Inference ***/
  
  def hasFunctionTyp(t: Typ): Boolean = t match {
    case TFunction(_, _) => true
    case TObj(fields) if (fields exists { case (_, t) => hasFunctionTyp(t) }) => true
    case _ => false
  }
    
  def mut(m: PMode): Mutability = m match {
    case PName => MConst
    case PVar | PRef => MVar
  }
  
  // Type Environment maps a variable name to a pair of mutability
  def typeInfer(env: Map[String,(Mutability,Typ)], e: Expr): Typ = {
    def typ(e1: Expr) = typeInfer(env, e1)
    def err[T](tgot: Typ, e1: Expr): T = throw new StaticTypeError(tgot, e1, e)

    e match {
      // TypePrint
      case Print(e1) => typ(e1); TUndefined      
      // TypeNumber
      case N(_) => TNumber      
      // TypeBool
      case B(_) => TBool
      // TypeUndefined
      case Undefined => TUndefined
      // TypeString
      case S(_) => TString
      // TypeNull
      case Null => TNull
      // TypeVar
      case Var(x) =>
        val (_, t) = env(x)
        t
      
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
      
      // TypePlusString
      case Binary(Plus, e1, e2) => typ(e1) match {
        case TNumber => typ(e2) match {
          case TNumber => TNumber
          case tgot => err(tgot, e2)
        }
        case TString => typ(e2) match {
          case TString => TString
          case tgot => err(tgot, e2)
        }
        case tgot => err(tgot, e1)
      }
      
      // TypeArith
      case Binary(Minus|Times|Div, e1, e2) => typ(e1) match {
        case TNumber => typ(e2) match {
          case TNumber => TNumber
          case tgot => err(tgot, e2)
        }
        case tgot => err(tgot, e1)
      }
      
      // TypeEquality
      case Binary(Eq|Ne, e1, e2) => typ(e1) match {
        case t1 if !hasFunctionTyp(t1) => typ(e2) match {
          case t2 if (t1 == t2) => TBool
          case tgot => err(tgot, e2)
        }
        case tgot => err(tgot, e1)
      }
      
      // TypeInequalityNumber && TypeInequalityString
      case Binary(Lt|Le|Gt|Ge, e1, e2) => typ(e1) match {
        case TNumber => typ(e2) match {
          case TNumber => TBool
          case tgot => err(tgot, e2)
        }
        case TString => typ(e2) match {
          case TString => TBool
          case tgot => err(tgot, e2)
        }
        case tgot => err(tgot, e1)
      }
      
      // TypeAndOr
      case Binary(And|Or, e1, e2) => typ(e1) match {
        case TBool => typ(e2) match {
          case TBool => TBool
          case tgot => err(tgot, e2)
        }
        case tgot => err(tgot, e1)
      }
      
      // TypeSeq
      case Binary(Seq, e1, e2) => 
        typ(e1); typ(e2)
        
      // TypeIf
      case If(e1, e2, e3) => typ(e1) match {
        case TBool =>
          val (t2, t3) = (typ(e2), typ(e3))
          if (t2 == t3) t2 else err(t3, e3)
        case tgot => err(tgot, e1)
      }
      
      // TypeObject
      // Type Object is a mapping to fields to types
      case Obj(fields) => 
        TObj(fields map { case (f,t) => (f, typ(t)) })
        
      // TypeGetField
      // Want to get a particular field name from object
      case GetField(e1, f) => {
         typ(e1) match {    
         	case TObj(tfields) if (tfields.contains(f)) => tfields(f)
         	case tgot => err(tgot, e1)
         }
      }
      
      // TypeDecl
      // declaration of mutable variables
      // mode specifies passing mode:name|var|ref
      case Decl(mode, x, e1, e2) => {
        typeInfer(env + (x -> (mode,typ(e1))), e2)
      }
       
      // TypeFunction      
      case Function(p, paramse, tann, e1) => {
        println("Function")
        // Bind to env1 an environment that extends env with an appropriate binding if
        // the function is potentially recursive.
        val env1 = (p, tann) match {
          case (Some(f), Some(rt)) =>
            val tprime = TFunction(paramse, rt)
            env + (f -> (MConst, tprime))
          case (None, _) => env
          case _ => err(TUndefined, e1)
        }
        
        // Bind to env2 an environment that extends env1 with the parameters.
        //We use can use env2 and store env1 in it then iterate through env2
        //to ultimately get our return type at the end of the day
        
        val env2 = paramse match {
          case Left(params) => params.foldLeft(env1) {
            case(envi, pari) => pari match {
              case(a, b) => envi + (a -> (MConst, b))
            }
          }
          case Right((mode,x,t)) => mode match {
            case PName => env1 + (x -> (MConst,t))
            case _ => env1 + (x -> (MVar,t))
          }
        }
        // Infer the type of the function body
        val t1 = typeInfer(env2, e1)
        tann foreach { rt => if (rt != t1) err(t1, e1) };
        TFunction(paramse, t1)
      }      

      // TypeCall
      case Call(e1, args) => typ(e1) match {
        // left = MConst
        case TFunction(Left(params), tret) if (params.length == args.length) => {
          (params, args).zipped.foreach {
            case((x, t), ex) => if (t != typ(ex)) err(t,ex)
          }
          tret
        }
        // right = defined
        case tgot @ TFunction(Right((mode,_,tparam)), tret) => mode match{
          case badcall if (args.length == 0) => err(tparam,e1)
          //if we don't have enough args return an error
          
          case (PName | PVar) => args.head match {
           case e => if (tparam == typ(e)) typ(e) else err(tparam, e1)
           //check to make sure the the type of parameters match what s given
          }
          case PRef => args.head match{
              case a => if (isLExpr(a) && tparam == typ(a)) tret else err(tparam, e1)
          }
        }
 
        case tgot => err(tgot, e1)
      }
      
      /*** Fill-in more cases here. ***/
      
      // TypeAssignVar && TypeAssignField
      case Assign(e1, e2) => e1 match {
		  // get variable type from env
      	case Var(x) => env.get(x) match {
			// return error on e2 if constant
			// otherwise map MVar and new type
      		case Some((MConst, t)) => err(typ(e1), e2)
      		case Some((MVar, t)) => if (t == typ(e2)) {
      			typeInfer(env + (x -> (MVar, typ(e2))), e2)
      		} else {
      			err(typ(e1), e2)
      		}
      		case _ => typeInfer(env + (x -> (MVar, typ(e2))), e2)
      	}
      	
      	// each field is assigned to a value
      	case GetField(x1, f) => typ(e1) match {
      		case t => typeInfer(env + (f -> (MConst, typ(e1))), e2)
      	}
      	case _ => err(typ(e1), e2)
      }
      
      // TypeCast
      case Unary(Cast(e1), e2) => (castOk(typ(e2), e1)) match{
		  // return e1 if valid cast, return error otherwise
        case true => e1;
        case false => err(typ(e2), e2);
        }

      /* Should not match: non-source expressions or should have been removed */
      case A(_) | Unary(Deref, _) | InterfaceDecl(_, _, _) => throw new IllegalArgumentException("Gremlins: Encountered unexpected expression %s.".format(e))
    }
  }
  
  /*** Small-Step Interpreter ***/
  
  /* Do the operation for an inequality. */
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
  
  /* Capture-avoiding substitution in e replacing variables x with esub. */
  def substitute(e: Expr, esub: Expr, x: String): Expr = {
    def subst(e: Expr): Expr = substitute(e, esub, x)
    val ep: Expr = avoidCapture(freeVars(esub), e)
    ep match {
      case N(_) | B(_) | Undefined | S(_) | Null | A(_) => e
      case Print(e1) => Print(subst(e1))
      case Unary(uop, e1) => Unary(uop, subst(e1))
      case Binary(bop, e1, e2) => Binary(bop, subst(e1), subst(e2))
      case If(e1, e2, e3) => If(subst(e1), subst(e2), subst(e3))
      case Var(y) => if (x == y) esub else e
      case Decl(mut, y, e1, e2) => Decl(mut, y, subst(e1), if (x == y) e2 else subst(e2))
      //SearchCall
      case Function(p, paramse, retty, e1) => {
        val ret = paramse match {
          case Left(params) => params.foldLeft(0: Int) {
        	  (ret: Int, c: (String, Typ)) => c match {
        	  	case (x2, t) if (x == x2) => ret + 1
        	  	case (x2, t) => ret}
          }
          case Right((_,name,_)) => x == name
        }
        if (p == Some(x) || (ret != 0)) {
          Function(p, paramse, retty, e1)
        }
        else {
          Function(p, paramse, retty, subst(e1))
        }
      }
   
      case Call(e1, args) => Call(subst(e1), args map subst)
      case Obj(fields) => Obj(fields map { case (fi,ei) => (fi, subst(ei)) })
      case GetField(e1, f) => GetField(subst(e1), f)
      case Assign(e1, e2) => Assign(subst(e1), subst(e2))
      case InterfaceDecl(tvar, t, e1) => InterfaceDecl(tvar, t, subst(e1))
    }
  }

  /* A small-step transition. */
  def step(e: Expr): DoWith[Mem, Expr] = {
	require(!isValue(e), "stepping on a value: %s".format(e))
    
	/*** Helpers for Call ***/
    
	def stepIfNotValue(e: Expr): Option[DoWith[Mem,Expr]] = if (isValue(e)) None else Some(step(e))
    
	/* Check whether or not the argument expression is ready to be applied. */
	def argApplyable(mode: PMode, arg: Expr): Boolean = mode match {
		case PVar => isValue(arg)
		case PName => true
		case PRef => isLValue(arg)
	}

	/*** Body ***/
    e match {
      /* Base Cases: Do Rules */
      // DoPrint
      case Print(v1) if isValue(v1) => for (m <- doget) yield { println(pretty(m, v1)); Undefined }
      // DoNeg
      case Unary(Neg, N(n1)) => doreturn( N(- n1) )
      // DoNot
      case Unary(Not, B(b1)) => doreturn( B(! b1) )
      // DoSeq
      case Binary(Seq, v1, e2) if isValue(v1) => doreturn( e2 )
      // DoPlusString
      case Binary(Plus, S(s1), S(s2)) => doreturn( S(s1 + s2) )
      // DoArith
      case Binary(Plus, N(n1), N(n2)) => doreturn( N(n1 + n2) )
      case Binary(Minus, N(n1), N(n2)) => doreturn( N(n1 - n2) )
      case Binary(Times, N(n1), N(n2)) => doreturn( N(n1 * n2) )
      case Binary(Div, N(n1), N(n2)) => doreturn( N(n1 / n2) )
      // DoInequalityNumber && DoInequalityString
      case Binary(bop @ (Lt|Le|Gt|Ge), v1, v2) if isValue(v1) && isValue(v2) => doreturn( B(inequalityVal(bop, v1, v2)) )
      // DoEquality
      case Binary(Eq, v1, v2) if isValue(v1) && isValue(v2) => doreturn( B(v1 == v2) )
      case Binary(Ne, v1, v2) if isValue(v1) && isValue(v2) => doreturn( B(v1 != v2) )
      // DoAndTrue && DoAndFalse
      case Binary(And, B(b1), e2) => doreturn( if (b1) e2 else B(false) )
      // DoOrTrue && DoOrFalse
      case Binary(Or, B(b1), e2) => doreturn( if (b1) B(true) else e2 ) 
      // DoIfTrue && DoIfFalse
      case If(B(b1), e2, e3) => doreturn( if (b1) e2 else e3 )
      
      // DoObject
      // Allocates memory for object and maps fields within the object
      // to address a
      case Obj(fields) if (fields forall { case (_, vi) => isValue(vi)}) =>
        Mem.alloc(Obj(fields)) map { (a:A) => a:Expr }        
        
        // DoGetField
        // Retrieves data of field f at address a
		case GetField(a @ A(_), v) => 
			// maps object address with DoWith object
			doget.map((m: Mem) => m.get(a) match {
			// does f exist?
			case Some(Obj(fields)) => fields.get(v) match {
				case Some(field) => field
				case _ => throw StuckError(e)
			}
			case _ => throw StuckError(e)
			})
			
      //DoCall && DoCallRec
      case Call(v1, args) if isValue(v1) =>
        def substfun(e1: Expr, p: Option[String]): Expr = p match {
          case None => e1
          case Some(x) => substitute(e1, v1, x)
        }
        
        // SearchCall2, SearchCallVar, SearchCallRed
        (v1, args) match {
		  // make sure number of params matches expected values and substitute
          case (Function(p, Left(params), tann, e1), args) if params.length == args.length => {
            doreturn(substfun((params, args).zipped.foldRight(e1){
              (vars: ((String, Typ), Expr), acc: Expr) => (vars, acc) match {
                case (((x, t), v1), e1) => substitute(e1, v1, x)
              }
            }, p))
          }
		  
		  // Pass by Value
          case (Function(p, Right((PVar, x1, _)), _, e1), v2 :: Nil) if isValue(v2) => {
			  // allocate memory for args and map on a
			  // substitute on e1 derefed with address and option sting
            Mem.alloc(v2) map {a => substfun(substitute(e1, Unary(Deref, a), x1), p)}
          }
          
          // Pass by Ref
          case (Function(p, Right((PRef, x1, _)), _, e1), lv2 :: Nil) if isLValue(lv2) => {
			  // return substituted function, with param name substituted with
			  // arg and p
            doreturn(substfun(substitute(e1, lv2, x1), p))
          }
          
          // Pass by Name
          case (Function(p, Right((PName, x1, _)), _, e1), e2 :: Nil) => {
			  // return substituted function, with all instances of param name 
			  // substituted with corresponding arg and p
            doreturn(substfun(substitute(e1, e2, x1), p))
          }

          // SearchCall
          case (Function(p, Right((PVar, x, _)), _, e1), e2 :: Nil) => {
            step(e2) map {e2p => Call(v1, e2p :: Nil)}
          }
          // SearchCallRef
          case (Function(p, Right((PRef, x1, _)), _, e1), e2 :: nil) => {
            step(e2) map {e2p => Call(v1, e2p :: Nil)}
          }
          case _ => throw StuckError(e)
        }
        
      // DoConst
      case Decl(MConst, x, v1, e2) if isValue(v1) =>
        doreturn(substitute(e2, v1, x))
      // DoVar
      case Decl(MVar, x, v1, e2) if isValue(v1) =>
        Mem.alloc(v1) map { a => substitute(e2, Unary(Deref, a), x)}

      // DoAssignVar
      case Assign(Unary(Deref, a @ A(_)), v) if isValue(v) =>
        // modify address to value and store in memory
        for (_ <- domodify { (m: Mem) => (m. + (a, v)): Mem }) yield v
        
      // DoAssignField
      case Assign(GetField(a @ A(_),f), v) if isValue(v) =>
        for(_ <- domodify {
		  // get memory at particular address if memory containes stored address of field
          (m: Mem) => {
            if(m.contains(a)){
              val obj = m(a)
              val newobj = obj match {
			    // map fields f to v
                case Obj(fields) => Obj( fields + (f -> (v)))
                case _ => throw StuckError(e)
              }
              m + (a -> newobj)
            }
            else m
          }
        }) 
          yield v
        
   /*** Fill-in more Do cases here. ***/
      
      // DoCast
      case Unary(Cast(e1), e2) => 
        if (e2 == Null) doreturn(Null) else doreturn(e2)

        
      // DoDeref
      case Unary(Deref, a @ A(_)) => doget.map((m: Mem) => m(a))
        
      /* Base Cases: Error Rules */
      /*** Fill-in cases here. ***/
        
      /* Inductive Cases: Search Rules */
      // SearchPrint
      case Print(e1) =>
        for (e1p <- step(e1)) yield Print(e1p)
      // SearchUnary
      case Unary(uop, e1) =>
        for (e1p <- step(e1)) yield Unary(uop, e1p)
      // SearchBinary1 && SearchBinary2
      case Binary(bop, v1, e2) if isValue(v1) =>
        for (e2p <- step(e2)) yield Binary(bop, v1, e2p)
      case Binary(bop, e1, e2) =>
        for (e1p <- step(e1)) yield Binary(bop, e1p, e2)
      // SearchIf
      case If(e1, e2, e3) =>
        for (e1p <- step(e1)) yield If(e1p, e2, e3)
      // SearchObject
      case Obj(fields) => fields find { case (_, ei) => !isValue(ei) } match {
      	case Some((fi, ei)) =>
      		for (eip <- step(ei)) yield Obj(fields + (fi -> eip))
      	case None => throw StuckError(e)
      }
      // SearchGetField
      case GetField(e1, f) => 
        // NullErrorGetField
        if(e1 == Null) throw new NullDereferenceError(e1)
        for (e1p <- step(e1)) yield GetField(e1p, f)
      
      
      /*** Fill-in more Search cases here. ***/
      // SearchDecl
      case Decl(mut, x, e1, e2) => {
    	  for (e1p <- step(e1)) yield Decl(mut, x, e1p, e2)
      }
      // SearchCall
      case Call(e1, e2) => {
    	  for (e1p <- step(e1)) yield Call(e1p, e2)
      }
      
      // SearchAssign2
      // use isLValue() to check for location values
      case Assign(e1, e2) if isLValue(e1) => {
    	  for (e2p <- step(e2)) yield Assign(e1, e2p)
      }
      
      // SearchAssign1
      case Assign(e1, e2) => {
    	  for (e1p <- step(e1)) yield Assign(e1p, e2)
      }
      
      /* Everything else is a stuck error. */
      case _ => throw StuckError(e)
    }
  }

  /*** External Interfaces ***/

  this.debug = true // comment this out or set to false if you don't want print debugging information
  this.maxSteps = Some(500) // comment this out or set to None to not bound the number of steps.
  
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
  
  case class TerminationError(e: Expr) extends Exception {
    override def toString = Parser.formatErrorMessage(e.pos, "TerminationError", "run out of steps in evaluating " + e)
  }
  
  def iterateStep(e: Expr): Expr = {
    require(closed(e), "not a closed expression: free variables: %s".format(freeVars(e)) )
    def loop(e: Expr, n: Int): DoWith[Mem,Expr] =
      if (Some(n) == maxSteps) throw TerminationError(e)
      else if (isValue(e)) doreturn( e )
      else {
        for {
          m <- doget[Mem]
          _ = if (debug) { println("Step %s:%n  %s%n  %s".format(n, m, e)) }
          ep <- step(e)
          epp <- loop(ep, n + 1)
        } yield
        epp
      }
    if (debug) {
      println("------------------------------------------------------------")
      println("Evaluating with step ...")
    }
    val (m,v) = loop(e, 0)(Mem.empty)
    if (debug) {
      println("Result:%n  %s%n  %s".format(m,v))
    }
    v
  }

  // Convenience to pass in a jsy expression as a string.
  def iterateStep(s: String): Expr = iterateStep(removeInterfaceDecl(jsy.lab5.Parser.parse(s)))
  
  // Interface for main
  def processFile(file: java.io.File) {
    if (debug) {
      println("============================================================")
      println("File: " + file.getName)
      println("Parsing ...")
    }
    
    val expr =
      handle(None: Option[Expr]) {Some{
        jsy.lab5.Parser.parseFile(file)
      }} getOrElse {
        return
      }
      
    val exprlowered =
      handle(None: Option[Expr]) {Some{
        removeInterfaceDecl(expr)
      }} getOrElse {
        return
      }  
    
    handle() {
      val t = inferType(exprlowered)
    }
    
    handle() {
      val v1 = iterateStep(exprlowered)
      println(pretty(v1))
    }
  }
    
}
