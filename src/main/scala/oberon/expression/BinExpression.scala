package oberon.expression

abstract class BinExpression(val lhs: Expression, val rhs: Expression) extends Expression {

}

class AddExpression(lhs: Expression, rhs: Expression) extends BinExpression(lhs, rhs) {

  override
  def eval() : Value = {
    val v1 : IntValue = lhs.eval().asInstanceOf[IntValue]
    val v2 : IntValue = rhs.eval().asInstanceOf[IntValue]

    return new IntValue(v1.value + v2.value)
  }
}

/** Subtraction is a add with a negative rhs */
class SubExpression(lhs: Expression, rhs: Expression) extends BinExpression(lhs, rhs) {

  override
  def eval() : Value = {
    var sum: AddExpression = new AddExpression( lhs, new IntValue(-1 * rhs.eval().asInstanceOf[IntValue].value) )
    return sum.eval
  }
}

class MulExpression(lhs: Expression, rhs: Expression) extends BinExpression(lhs, rhs) {

  override
  def eval() : Value = {
    val v1 : IntValue = lhs.eval().asInstanceOf[IntValue]
    val v2 : IntValue = rhs.eval().asInstanceOf[IntValue]

    return new IntValue(v1.value * v2.value)
  }
}

class DivExpression(lhs: Expression, rhs: Expression) extends BinExpression(lhs, rhs) {

  override
  def eval() : Value = {
    val v1: IntValue = rhs.eval().asInstanceOf[IntValue]
    if(v1.eval == IntValue(0))
        throw new oberon.InvalidArgument("Invalid rhs for requested operation")
    
    return new IntValue(lhs.eval.asInstanceOf[IntValue].value / v1.value)
  }
}

/** Strict Inequality: Greater Than */
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

  override
  def eval: Value = {
    val v1 = lhs.eval().asInstanceOf[IntValue].value
    val v2 = rhs.eval().asInstanceOf[IntValue].value

    return BoolValue(v1 <= v2) 
  }

}

class EqExpression(lhs: Expression, rhs: Expression) extends BinExpression(lhs, rhs) {

  override
  def eval: Value = {
    val v1 = lhs.eval()
    val v2 = rhs.eval()

    return BoolValue(v1 == v2) 
  }

}

class NeqExpression(lhs: Expression, rhs: Expression) extends BinExpression(lhs, rhs) {

  override
  def eval: Value = {
    var value: EqExpression = new EqExpression(lhs,rhs)
    if( value.eval == BoolValue(true) )
        return BoolValue(false)
    else
        return BoolValue(true)
  }

}


class AND_Expression(lhs: Expression, rhs: Expression) extends BinExpression(lhs, rhs) {

  override
  def eval() : Value = {
    val v1 : BoolValue = lhs.eval().asInstanceOf[BoolValue]
    val v2 : BoolValue = rhs.eval().asInstanceOf[BoolValue]

    return new BoolValue(v1.value && v2.value)
  }
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