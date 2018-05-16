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

  behavior of "a for command"

  before {
    clear()
  }
  // x;
  // soma = 0;
  // for(x = 1;x < 11; x++)
  //   soma := soma + x;
  // end
  // print(soma);  
  it should "lookup(soma) must be equal to 55 after a loop summing up 1 to 10" in {
    val d1 = new Declaration("x") // x;
    val a1 = new Assignment("soma", IntValue(0))     // soma := 0;
    val a2 = new Assignment("soma",new AddExpression(new VarRef("soma"), new VarRef("x")))
    val a3 = new Assignment("x", IntValue(1))
    val a4 = new Assignment("x",new AddExpression(new VarRef("x"),IntValue(1)))
    val cond = new SlExpression(new VarRef("x"), IntValue(11))
    val f1 = new For(new BlockCommand(List(d1,a3)),cond, new BlockCommand(List(a4)), new BlockCommand(List(a2)))

    a1.run()
    f1.run()

    val res = lookup("soma")
    res match {
      case Some(v) => v.eval() should be (IntValue(55))
      case _       => 5 should be (1)  
    }
  }
}
