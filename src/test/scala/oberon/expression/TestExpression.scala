package oberon.expression

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.GivenWhenThen
import org.scalatest.BeforeAndAfter


class TestExpression extends FlatSpec with Matchers with GivenWhenThen with BeforeAndAfter {

  behavior of "a Expression"

  var greaterOrEqual   : Expression = _
  var lessOrEqual      : Expression = _

  var greaterThan      : Expression = _
  var lessThan         : Expression = _

  var notEqual         : Expression = _
  var equal            : Expression = _
  
  it should "return value 5 in IntValue(5).eval" in {
    val val5 = new IntValue(5)

    val5.eval should be (IntValue(5)) 
  }
  
  it should "return value false in BoolValue(false).eval" in {
    val valf = new BoolValue(false)

    valf.eval should be (BoolValue(false))
  }

  it should "have Sg( Value(X), Value(Y) ) == BoolValue(true), if X >  Y" in {    
    greaterThan = new SgExpression( IntValue(15), IntValue(10) )
    greaterThan.eval() should be (BoolValue(true)) 
    
    greaterThan = new SgExpression( IntValue(10), IntValue(10) )
    greaterThan.eval() should be (BoolValue(false)) 
    
    greaterThan = new SgExpression( IntValue( 5), IntValue(10) ) 
    greaterThan.eval() should be (BoolValue(false))
    
  }

  it should "have Sl( Value(X), Value(Y) ) == BoolValue(true), if X <  Y" in {
    lessThan    = new SlExpression( IntValue(15), IntValue(10) )
    lessThan.eval() should be (BoolValue(false)) 
    
    lessThan    = new SlExpression( IntValue(10), IntValue(10) ) 
    lessThan.eval() should be (BoolValue(false))
    
    lessThan    = new SlExpression( IntValue( 5), IntValue(10) ) 
    lessThan.eval() should be (BoolValue(true))
  }

  it should "have Ge( Value(X), Value(Y) ) == BoolValue(true), if X >= Y" in {
    greaterOrEqual    = new GeExpression( IntValue(15), IntValue(10) )
    greaterOrEqual.eval() should be (BoolValue(true)) 
    
    greaterOrEqual    = new GeExpression( IntValue(10), IntValue(10) ) 
    greaterOrEqual.eval() should be (BoolValue(true))
    
    greaterOrEqual    = new GeExpression( IntValue( 5), IntValue(10) ) 
    greaterOrEqual.eval() should be (BoolValue(false))
  }

  it should "have Le( Value(X), Value(Y) ) == BoolValue(true), if X <= Y" in {
    lessOrEqual    = new LeExpression( IntValue(15), IntValue(10) )
    lessOrEqual.eval() should be (BoolValue(false)) 
    
    lessOrEqual    = new LeExpression( IntValue(10), IntValue(10) ) 
    lessOrEqual.eval() should be (BoolValue(true))
    
    lessOrEqual    = new LeExpression( IntValue( 5), IntValue(10) ) 
    lessOrEqual.eval() should be (BoolValue(true))
  }

  it should "have Eq( Value(X), Value(Y) ) == BoolValue(true), if X == Y" in {  
    equal    = new EqExpression( IntValue(15), IntValue(10) )
    equal.eval() should be (BoolValue(false))
    
    equal    = new EqExpression( IntValue(10), IntValue(10) ) 
    equal.eval() should be (BoolValue(true))
    
    equal    = new EqExpression( IntValue( 5), IntValue(10) ) 
    equal.eval() should be (BoolValue(false))

    equal    = new EqExpression( IntValue(5), new AddExpression(IntValue(3), IntValue(2)) )
    equal.eval() should be (BoolValue(true))
  }

  it should "have Neq(Value(X), Value(Y) ) == BoolValue(true), if X != Y" in {  
    notEqual    = new NeqExpression( IntValue(15), IntValue(10) )
    notEqual.eval() should be (BoolValue(true)) 
    
    notEqual    = new NeqExpression( IntValue(10), IntValue(10) ) 
    notEqual.eval() should be (BoolValue(false))
    
    notEqual    = new NeqExpression( IntValue( 5), IntValue(10) ) 
    notEqual.eval() should be (BoolValue(true))

    notEqual    = new NeqExpression( IntValue(6), new AddExpression(IntValue(3), IntValue(2)) )
    notEqual.eval() should be (BoolValue(true)) 
  }

}
