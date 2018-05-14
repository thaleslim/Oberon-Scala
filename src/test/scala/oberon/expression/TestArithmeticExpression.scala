package oberon.expression

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.GivenWhenThen
import org.scalatest.BeforeAndAfter


class TestArithmeticExpression extends FlatSpec with Matchers with GivenWhenThen with BeforeAndAfter {

  behavior of "Arithmetic expressions"
    //TODO: expandir testes
  it should "return value 15 in Add(IntValue(5), IntValue(10))" in {
    val val5  = IntValue(5)
    val val10 = IntValue(10)
    val add   = new AddExpression(val5, val10) 

    add.eval() should be (IntValue(15)) 
  }

  it should "lead to an exception in Add(IntValue(5), BoolValue(False))" in {
    val val5 = IntValue(5)
    val valf = BoolValue(false)
    val add = new AddExpression(val5, valf)

    //Since a BoolValue cannot be casted to IntValue
    intercept[java.lang.ClassCastException]{
        add.eval() should be (IntValue(5))
    }
  }

  it should "return value 15 in Sub(IntValue(20), IntValue(5))" in {
    val val20  = IntValue(20)
    val val5   = IntValue(5)
    val add    = new SubExpression(val20, val5) 

    add.eval() should be (IntValue(15))
  }

  it should "lead to an exception in Sub(IntValue(5), BoolValue(False))" in {
    val val5 = IntValue(5)
    val valf = BoolValue(false)
    val add = new SubExpression(val5, valf)
    
    //Since a BoolValue cannot be casted to IntValue
    intercept[java.lang.ClassCastException]{
        add.eval() should be (IntValue(5))
    }
  }
}
