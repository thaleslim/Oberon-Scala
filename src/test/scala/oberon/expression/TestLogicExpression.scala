package oberon.expression

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.GivenWhenThen
import org.scalatest.BeforeAndAfter


class TestLogicExpression extends FlatSpec with Matchers with GivenWhenThen with BeforeAndAfter {

  behavior of "Logic expressions (AND,OR,NOT)"
  
  it should "only return true if both expressions are true, in other words, AND operation" in {
    //00
    val and0   = new AND_Expression(BoolValue(false),BoolValue(false))
    and0.eval() should be (BoolValue(false))
    //01
    val and1   = new AND_Expression(BoolValue(false),BoolValue(true))
    and1.eval() should be (BoolValue(false))
    //10
    val and2   = new AND_Expression(BoolValue(true),BoolValue(false))
    and2.eval() should be (BoolValue(false))
    //11
    val and3   = new AND_Expression(BoolValue(true),BoolValue(true))
    and3.eval() should be (BoolValue(true))
  }

  it should "return true if at least one expression is true, in other words, OR operation" in {
    //00
    val or0   = new OR_Expression(BoolValue(false),BoolValue(false))
    or0.eval() should be (BoolValue(false))
    //01
    val or1   = new OR_Expression(BoolValue(false),BoolValue(true))
    or1.eval() should be (BoolValue(true))
    //10
    val or2   = new OR_Expression(BoolValue(true),BoolValue(false))
    or2.eval() should be (BoolValue(true))
    //11
    val or3   = new OR_Expression(BoolValue(true),BoolValue(true))
    or3.eval() should be (BoolValue(true))
  }

  it should "return true if the expression is false and vice versa,  a.k.a, NOT operation" in {
    //0
    val not0   = new NOT_Expression(BoolValue(false))
    not0.eval() should be (BoolValue(true))
    //1
    val not1   = new NOT_Expression(BoolValue(true))
    not1.eval() should be (BoolValue(false))
  }

}
