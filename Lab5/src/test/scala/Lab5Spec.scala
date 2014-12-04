import org.scalatest._
import jsy.lab5.ast._
import Lab5._

class Lab5Spec extends FlatSpec {
  
  "mapFirstDoWith" should "map the first element where f returns Some" in {
     val l1 = List(1, 2, -3, 4, -5)
     val gold1 = List(1, 2, 3, 4, -5)
     def dowith[W]: DoWith[W,List[Int]] = mapFirstWith[W,Int] { (i: Int) => if (i < 0) Some(doreturn(-i)) else None } (l1)
     assertResult((true,gold1)) { dowith(true) }
     assertResult((42,gold1)) { dowith(42) }
  }

  "DoNeg" should "return the negative of a number" in {
     val e = Unary(Neg, N(40))
     val (mp:Mem, ep:Expr) = step(e)(Mem.empty)
     assertResult(N(-40)) { ep }
  }

  "DoNot" should "return the negative of a boolean" in {
     val e = Unary(Not, B(true))
     val (mp:Mem, ep:Expr) = step(e)(Mem.empty)
     assertResult(B(false)) { ep }
  }

  "DoPlus" should "return the addition of two numbers" in {
     val e = Binary(Plus, N(5), N(4))
     val (mp:Mem, ep:Expr) = step(e)(Mem.empty)
     assertResult(N(9)) { ep }
  }

  "DoMinus" should "return the difference of two numbers" in {
     val e = Binary(Minus, N(5), N(4))
     val (mp:Mem, ep:Expr) = step(e)(Mem.empty)
     assertResult(N(1)) { ep }
  }
  
    "DoTimes" should "return the product of two numbers" in {
     val e = Binary(Times, N(5), N(4))
     val (mp:Mem, ep:Expr) = step(e)(Mem.empty)
     assertResult(N(20)) { ep }
  }
  
  "DoDiv" should "return the divison of two numbers" in {
     val e = Binary(Div, N(8), N(4))
     val (mp:Mem, ep:Expr) = step(e)(Mem.empty)
     assertResult(N(2)) { ep }
  }

  // Probably want to write some tests for castOk, typeInfer, substitute, and step.
}
