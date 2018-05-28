package oberon.expression

/** General interface to a Binary Expression.
  
  * This is a expression composed by 2 others.
  */
abstract class BinExpression(val lhs: Expression, val rhs: Expression) extends Expression {
}

/** Sum ( + ) representation.

  * Adds two expression' values.
  */
class AddExpression(lhs: Expression, rhs: Expression) extends BinExpression(lhs, rhs) {

    /** lhs + rhs */
    override def eval() : Value =
        IntValue( lhs.eval().asInstanceOf[IntValue].value + rhs.eval().asInstanceOf[IntValue].value )
}

/** sub ( - ) representation.
  
  * A subtraction is in essence a addition with a negative term.
  */
class SubExpression(lhs: Expression, rhs: Expression) extends BinExpression(lhs, rhs) {

    /** lhs - rhs == lhs + (-rhs) */
    override def eval() : Value = 
        ( new AddExpression( lhs, new IntValue( -1 * rhs.eval().asInstanceOf[IntValue].value ) ) ).eval
}

/** mul ( x or · or * ) representation.

  * A multiplication reduces two expression to a single number: the product
  * between a multiplicand and a multiplier, also known as factors.

  * The product is the result of a addition of a multiplicand repeated n-1 
  * times with itself being n the multiplier.
  */
class MulExpression(lhs: Expression, rhs: Expression) extends BinExpression(lhs, rhs) {
    
    /** lhs · rhs */
    override def eval() : Value =
        IntValue( lhs.eval().asInstanceOf[IntValue].value * rhs.eval().asInstanceOf[IntValue].value )
}

/** div (÷ or /) representation.
  
  * A division is in essence the inverse of multiplication.

  * Division results in a quotient from two expression' values,
  * the dividend by the divisor.
  */
class DivExpression(lhs: Expression, rhs: Expression) extends BinExpression(lhs, rhs) {

    /** lhs ÷ rhs 
      * @throws InvalidArgument rhs == 0? throw else proceed
      */
    override def eval() : Value = {
        val v1: IntValue = rhs.eval().asInstanceOf[IntValue]
        if(v1.eval == IntValue(0))
            throw new oberon.InvalidArgument("Invalid rhs for requested operation")
        
        return new IntValue(lhs.eval.asInstanceOf[IntValue].value / v1.value)
    }
}

/** mod (%) representation.
  
  * Modulus returns the remainder from a division.
  */
class ModExpression(lhs: Expression, rhs: Expression) extends BinExpression(lhs, rhs) {

    /** lhs % rhs 
      * @throws InvalidArgument rhs == 0? throw else proceed
      */
    override def eval() : Value = {
        val v1: IntValue = rhs.eval().asInstanceOf[IntValue]
        if(v1.eval == IntValue(0))
            throw new oberon.InvalidArgument("Invalid rhs for requested operation")

        return new IntValue(lhs.eval.asInstanceOf[IntValue].value % v1.value)
    }
}

/** < representation 
  
  * Strict Inequality: Greater Than
  */
class SgExpression(lhs: Expression, rhs: Expression) extends BinExpression(lhs, rhs) {

  override
  def eval: Value = {
    var lessEqual: LeExpression = new LeExpression(lhs,rhs)
    if( lessEqual.eval == BoolValue(true) )
        return BoolValue(false)
    else
        return BoolValue(true)
  }

}

/** Strict Inequality: Less Than */
class SlExpression(lhs: Expression, rhs: Expression) extends BinExpression(lhs, rhs) {

  override
  def eval: Value = {
    var greaterEqual: GeExpression = new GeExpression(lhs,rhs)
    if( greaterEqual.eval == BoolValue(true) )
        return BoolValue(false)
    else
        return BoolValue(true)
  }

}
/** Inequality Expression: Greater than or Equal to */
class GeExpression(lhs: Expression, rhs: Expression) extends BinExpression(lhs, rhs) {

  override
  def eval: Value = {
    val v1 = lhs.eval().asInstanceOf[IntValue].value
    val v2 = rhs.eval().asInstanceOf[IntValue].value

    return BoolValue(v1 >= v2)
  }

}

class LeExpression(lhs: Expression, rhs: Expression) extends BinExpression(lhs, rhs) {

    /** lhs <= rhs */
    override def eval: Value = 
        BoolValue( lhs.eval().asInstanceOf[IntValue].value <= rhs.eval().asInstanceOf[IntValue].value )

}

class EqExpression(lhs: Expression, rhs: Expression) extends BinExpression(lhs, rhs) {
    /** */
    override def eval: Value = BoolValue( lhs.eval == rhs.eval ) 

}

/** Not Equal ( != ) representation
  */
class NeqExpression(lhs: Expression, rhs: Expression) extends BinExpression(lhs, rhs) {
    
    /** lhs != rhs */
    override def eval: Value =
        if( (new EqExpression(lhs,rhs)).eval == BoolValue(true) ) BoolValue(false) else BoolValue(true)

}

class AND_Expression(lhs: Expression, rhs: Expression) extends BinExpression(lhs, rhs) {

    override def eval() : Value =
        BoolValue( lhs.eval().asInstanceOf[BoolValue].value && rhs.eval().asInstanceOf[BoolValue].value)
}

class OR_Expression(lhs: Expression, rhs: Expression) extends BinExpression(lhs, rhs) {

  override
  def eval() : Value = {
    val v1 : BoolValue = lhs.eval().asInstanceOf[BoolValue]
    val v2 : BoolValue = rhs.eval().asInstanceOf[BoolValue]

    return new BoolValue(v1.value || v2.value)
  }
}

class NOT_Expression(v1: Expression) extends Expression {

  override
  def eval() : Value = new BoolValue( ! v1.eval().asInstanceOf[BoolValue].value )
}