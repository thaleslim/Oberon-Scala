package oberon.command

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.GivenWhenThen
import org.scalatest.BeforeAndAfter


import oberon.Environment._

import oberon.expression.IntValue
import oberon.expression._
import oberon.command._

class TestDeclaration extends FlatSpec with Matchers with GivenWhenThen with BeforeAndAfter {

  behavior of "a declaration command"

  before {
    clear()
  }
  // x;
  // print(x)  
  it should "lookup(x) == Undefined()" in {
    val a1 = new Declaration("x")    //    x;
    a1.run()
    val res = lookup("x")
    res match {
      case Some(v) => v.eval() should be (Undefined())
      case _       => 5 should be (1)
    }
  }
}
