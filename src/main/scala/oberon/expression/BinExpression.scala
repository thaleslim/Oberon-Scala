package oberon.expression

/** General interface to a Binary Expression.
  
  * This is a expression composed by 2 others.
  */
abstract class BinExpression(val lhs: Expression, val rhs: Expression) extends Expression {
}

/** sum ( + ) representation.

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

/** Strictly greater than ( > ) representation.
  
  * Strict Inequality Expression
  */
class SgExpression(lhs: Expression, rhs: Expression) extends BinExpression(lhs, rhs) {

    /** lhs > that == !(lhs <= rhs) */
    override def eval: Value = 
        if( (new LeExpression(lhs,rhs)).eval == BoolValue(true) ) BoolValue(false) else BoolValue(true)

}

/** Strictly less than ( < ) representation. 
  
  * Strict Inequality Expression
  */
class SlExpression(lhs: Expression, rhs: Expression) extends BinExpression(lhs, rhs) {

    /** this < that == !(lhs >= rhs) */
    override def eval: Value =
        if( (new GeExpression(lhs,rhs)).eval == BoolValue(true) ) BoolValue(false) else BoolValue(true)

}

/** Greater than or Equal to ( >= ) representation. */
class GeExpression(lhs: Expression, rhs: Expression) extends BinExpression(lhs, rhs) {

    /** lhs >= rhs */
    override def eval: Value = 
        BoolValue( lhs.eval().asInstanceOf[IntValue].value >= rhs.eval().asInstanceOf[IntValue].value )

}

/** Less than or Equal to ( <= ) representation. */
class LeExpression(lhs: Expression, rhs: Expression) extends BinExpression(lhs, rhs) {

    /** lhs <= rhs */
    override def eval: Value = 
        BoolValue( lhs.eval().asInstanceOf[IntValue].value <= rhs.eval().asInstanceOf[IntValue].value )

}

/** Equal to ( == ) representation. */
class EqExpression(lhs: Expression, rhs: Expression) extends BinExpression(lhs, rhs) {
    
    /** lhs == rhs */
    override def eval: Value = BoolValue( lhs.eval == rhs.eval ) 

}

/** Not Equal ( != ) representation. */
class NeqExpression(lhs: Expression, rhs: Expression) extends BinExpression(lhs, rhs) {
    
    /** lhs != rhs */
    override def eval: Value =
        if( (new EqExpression(lhs,rhs)).eval == BoolValue(true) ) BoolValue(false) else BoolValue(true)

}

/** and ( && ) representation. */
class AND_Expression(lhs: Expression, rhs: Expression) extends BinExpression(lhs, rhs) {

    /** lhs && rhs */
    override def eval() : Value =
        BoolValue( lhs.eval().asInstanceOf[BoolValue].value && rhs.eval().asInstanceOf[BoolValue].value )
}

/** or ( || ) representation. */
class OR_Expression(lhs: Expression, rhs: Expression) extends BinExpression(lhs, rhs) {

    override def eval() : Value = 
        BoolValue( lhs.eval().asInstanceOf[BoolValue].value || rhs.eval().asInstanceOf[BoolValue].value )
}

/** not ( ! ) representation. */
class NOT_Expression(expression: Expression) extends Expression {

    /** !expression */
    override def eval() : Value = BoolValue( !expression.eval().asInstanceOf[BoolValue].value )
}