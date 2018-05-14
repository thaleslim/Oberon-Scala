package oberon.expression

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.GivenWhenThen
import org.scalatest.BeforeAndAfter


class TestArithmeticExpression extends FlatSpec with Matchers with GivenWhenThen with BeforeAndAfter {

  behavior of "Arithmetic expressions"
  
  var result: Expression = _

  it should "return value X+Y in Add(IntValue(X), IntValue(Y)).eval" in {
    result   = new AddExpression(IntValue(5), IntValue(10)) 
    result.eval should be (IntValue(15))
    
    result   = new AddExpression(IntValue(5), IntValue(-10)) 
    result.eval should be (IntValue(-5))
    
    result   = new AddExpression(IntValue(2000), IntValue(1))
    result.eval should be (IntValue(2001))
  }

  it should "return value X-Y in Sub(IntValue(X), IntValue(Y)).eval" in {
    result   = new SubExpression(IntValue(5), IntValue(10)) 
    result.eval should be (IntValue(-5))
    
    result   = new SubExpression(IntValue(5), IntValue(-10)) 
    result.eval should be (IntValue(15))
    
    result   = new SubExpression(IntValue(2000), IntValue(1))
    result.eval should be (IntValue(1999))
  }

  it should "return value X*Y in Mul(IntValue(X), IntValue(Y)).eval" in {
    result   = new MulExpression(IntValue(5), IntValue(10)) 
    result.eval should be (IntValue(50))
    
    result   = new MulExpression(IntValue(5), IntValue(-10)) 
    result.eval should be (IntValue(-50))
    
    result   = new MulExpression(IntValue(2000), IntValue(1))
    result.eval should be (IntValue(2000))
  }

  it should "return value X/Y in Div(IntValue(X), IntValue(Y)).eval" in {
    result   = new DivExpression(IntValue(5), IntValue(10)) 
    result.eval should be (IntValue(0))
    
    result   = new DivExpression(IntValue(10), IntValue(2)) 
    result.eval should be (IntValue(5))
    
    result   = new DivExpression(IntValue(2000), IntValue(1))
    result.eval should be (IntValue(2000))
  }

  it should "lead to an exception in Add(IntValue(X), BoolValue(Y)).eval, being X a Integer and Y a Boolean" in {
    result = new AddExpression(IntValue(5), BoolValue(false))

    //Since a BoolValue cannot be casted to IntValue
    intercept[java.lang.ClassCastException]{
        result.eval() should be (IntValue(5))
    }
  }

  it should "lead to an exception in Div(IntValue(X), IntValue(0)).eval" in {
    result = new DivExpression(IntValue(5), IntValue(0))

    //Since a BoolValue cannot be casted to IntValue
    intercept[oberon.InvalidArgument]{
        result.eval() should be (IntValue(0))
    }
  }
}
