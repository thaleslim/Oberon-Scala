package oberon.command

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.GivenWhenThen
import org.scalatest.BeforeAndAfter


import oberon.Environment._

//import oberon.expression.IntValue
import oberon.expression._
import oberon.command._

class TestProcedure extends FlatSpec with Matchers with GivenWhenThen with BeforeAndAfter {

  behavior of "a Procedure command"

  before {
    clear()
  }
  // soma;
  // procedure for
  // begin
  //    for(soma := 0, x := 1; x < 11; x++) begin
  //      soma := soma + x;
  //    end
  // end
  // 
  // begin
  //    print(soma);
  // end
  //
  // or ...
  // for(soma := 0, x := 1; x < 11; {x++; soma := soma + x;})
  it should "lookup(soma) must be equal to 55 after a loop summing up 1 to 10" in {
    val d1 = new Declaration("soma")                 // soma;
    
    val a1 = new Assignment("soma", IntValue(0))     // soma := 0;
    val a2 = new Assignment("x", IntValue(1))        //    x := 1;
    val a3 = new Assignment("soma",new AddExpression(new VarRef("soma"), new VarRef("x")))
    val a4 = new Assignment("x", new AddExpression(new VarRef("x"), IntValue(1)))
    val cond = new SlExpression(new VarRef("x"), IntValue(11))
    
    val f1 = new For(new BlockCommand(List(a1,a2)), cond, new BlockCommand(List(a3, a4)))

    val procedure = new Procedure(new BlockCommand(List(f1)))

    d1.run()
    procedure.declare("for")
    (new ProcedureCall("for")).run()

    val sum = lookup("soma")
    sum match {
      case Some(v) => v.eval() should be (IntValue(55))
      case _       => -1 should be (0)  
    }
  }
  // soma;
  //
  // procedure assign(x: Int)
  // begin
  //    soma := x;
  // end
  // 
  // begin
  //    print(soma);
  // end
  //
  it should "Assign the Value from the Argument X to the Global Variable Sum" in {
    val d1 = new Declaration("soma")                 // soma;

    val a1 = new Assignment("soma", new VarRef("x")) // soma := x;

    (new Procedure(new BlockCommand(List(a1)), ("x" -> new Variable("Int")))).declare("assign") // procedure assign(x: Int)

    d1.run()
    (new ProcedureCall("assign",("x" -> (new Variable)(IntValue(5))))).run()

    val sum = lookup("soma")
    sum match {
      case Some(v) => v.eval() should be (IntValue(5))
      case _       => -1 should be (0)  
    }
  }
}
