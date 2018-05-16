package oberon.command

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.GivenWhenThen
import org.scalatest.BeforeAndAfter


import oberon.Environment._

import oberon.expression.IntValue
import oberon.expression._
import oberon.command._

class TestIfThen extends FlatSpec with Matchers with GivenWhenThen with BeforeAndAfter {

  behavior of "a If command"

  before {
    clear()
  }
  // result := 0;
  //    x   := 1;
  // if(result <= 10) then begin
  //      x := x + 1;
  // end
  // print(result);
  it should "lookup(x) must be equal to 2 after a if statement, case true, summing 1 to x" in {
    val a1 = new Assignment("result", IntValue(0))     // result := 0;
    val a2 = new Assignment("x", IntValue(1))          //    x   := 1;
    val a3 = new Assignment("x", new AddExpression(new VarRef("x"), IntValue(1)))
    val cond = new LeExpression(new VarRef("result"), IntValue(10))
    val if1 = new IfThen(cond, new BlockCommand(List(a3)))

    a1.run()
    a2.run()
    if1.run()

    val res = lookup("x")
    res match {
      case Some(v) => v.eval() should be (IntValue(2))
      case _       => 5 should be (1)
    }
  }
}
