package oberon.expression

trait Expression {
  def eval(): Value 
}

trait Value extends Expression {
  def eval() = this 
}

case class Undefined() extends Value 
case class IntValue(value: Integer) extends Value
case class BoolValue(value: Boolean) extends Value

case class Variable() extends Value with Comparable[Variable]{
    var valueType: String = "Undefined"
    var value: Value = new Undefined()

    def apply(info: Value): Variable = this.assign(info)

    def assign(info: Value): Variable = {
        info.eval match {
            case IntValue(value) =>
                if( this.valueType == "Undefined" ){
                    this.value = new IntValue(value)
                    this.valueType  = "Int" }
                else if ( this.valueType == "Int" )
                    this.value = new IntValue(value)
                else throw new oberon.InvalidArgument("Invalid Type")

            case BoolValue(value) =>
                if( this.valueType == "Undefined" ){
                    this.value = new BoolValue(value)
                    this.valueType  = "Bool";}
                else if ( this.valueType == "Bool" )
                    this.value = new BoolValue(value)
                else throw new oberon.InvalidArgument("Invalid Type")

            case Undefined() => {}
            case _ => throw new  oberon.InvalidArgument("Unexpected Type")
        }
        return this
    }
    /**
      * @return a negative integer, zero, or a positive integer as this 
      * object is less than, equal to, or greater than the specified object.
      */
    def compareTo(that: Variable): Int = {
        if( !(this.valueType == that.valueType) ) return -1
        else this.value.eval match{
            case Undefined() => that.value.eval match {
                case Undefined() => return 0
                case _ => return -1
            }
            case IntValue(thisInt) => that.value.eval match {
                case IntValue(thatInt) => {
                    if(thisInt < thatInt) return -1 
                    else if(thisInt == thatInt) return 0
                    else return 1
                }
                case _ => return -1
            }
            case BoolValue(thisBool) => that.value.eval match {
                case BoolValue(thatBool) => 
                    if( !thisBool && thatBool ) return -1
                    else if( thisBool && thatBool || !thisBool && !thatBool ) return 0
                    else return 1
                case _ => return -1
            }
        }
    }

    def equalTo(that: Variable): Boolean = if( this.compareTo(that) == 0 ) true else false

}