package oberon.command

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.GivenWhenThen
import org.scalatest.BeforeAndAfter


import oberon.Environment._

import oberon.expression.IntValue
import oberon.expression._
import oberon.command._

class TestIfThenElse extends FlatSpec with Matchers with GivenWhenThen with BeforeAndAfter {

  behavior of "a IfThenElse command"

  before {
    clear()
  }
  // x;
  // if(true) then begin
  //      x := 1;
  // end else begin
  //      x := 2;
  // end
  // print(soma);  
  it should "lookup(x) must be equal to 1 after a IfThenElse with a true condition" in {
    val a1 = new Declaration("x", new IntValue(32))  //    x;
    val a2 = new Assignment("x", IntValue(1))        //    x := 1;
    val a3 = new Assignment("x", IntValue(2))        //    x := 2;
    val cond = new EqExpression(IntValue(1), IntValue(1))
    val ifelse1 = new IfThenElse(cond, new BlockCommand(List(a2)), new BlockCommand(List(a3)))

    a1.run()
    a2.run()
    ifelse1.run()

    val res = lookup("x")
    res match {
      case Some(v) => v.eval() should be (IntValue(1))
      case _       => 5 should be (1)  
    }
  }
}
