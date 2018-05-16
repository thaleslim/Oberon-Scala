package oberon.command

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.GivenWhenThen
import org.scalatest.BeforeAndAfter


import oberon.Environment._

import oberon.expression.IntValue
import oberon.expression._
import oberon.command._

class TestFor extends FlatSpec with Matchers with GivenWhenThen with BeforeAndAfter {

  behavior of "a For command"

  before {
    clear()
  }
  // soma;
  // for(soma := 0, x := 1; x <= 10; x++) begin
  //   soma := soma + x;
  // end
  // print(soma);
  it should "lookup(soma) must be equal to 55 after a loop summing up 1 to 10" in {
    val d1 = new Declaration("soma")                 // soma;
    val a1 = new Assignment("soma", IntValue(0))     // soma := 0;
    val a2 = new Assignment("x", IntValue(1))        //    x := 1;
    val a3 = new Assignment("soma",new AddExpression(new VarRef("soma"), new VarRef("x")))
    val a4 = new Assignment("x", new AddExpression(new VarRef("x"), IntValue(1)))
    val cond = new SlExpression(new VarRef("x"), IntValue(11))
    val f1 = new For(new BlockCommand(List(a1,a2)), cond, new BlockCommand(List(a3, a4)))

    d1.run()
    f1.run()

    val sumResult = lookup("soma")
    sumResult match {
      case Some(v) => v.eval() should be (IntValue(55))
      case _       => -1 should be (0)  
    }

    val garbage = lookup("x")
    garbage match {
      case None => 40 should be (40)
      case _ => 0 should be (1)
    }

  }
}
