package oberon.expression

/** A expression representation.
  
  * This should, by definition, be able to be reduced to a value.
  */
trait Expression {
    def eval(): Value
}

/** A generic value representation. */
trait Value extends Expression {
    def eval() = this 
}

/** A Undefined value representation.
  
  * @note It's used as a default value
  */
case class Undefined() extends Value 
/** A Int value representation. */
case class IntValue(value: Integer) extends Value
/** A Boolean value representation. */
case class BoolValue(value: Boolean) extends Value

/** A variable representation.

  * @note this' valueType 'n value are only visible for debugging purposes, avoid as much as possible using them directly!!!
  */
case class Variable( var valueType: String = "Undefined", var value: Value = new Undefined() ) extends Value with Ordered[Variable]{

    override 
    def eval() = this.value

    def apply     (info: Value)   : Variable = this.assign(info)

    def assign    (info: Value)   : Variable = {
        info.eval match {
            case IntValue(value) => 
                if( this.valueType == "Undefined" ){
                    this.value = new IntValue(value)
                    this.valueType  = "Int"}
                else if( this.valueType == "Int" )
                    this.value = new IntValue(value)
                else throw new oberon.InvalidArgument("Assign: Invalid Type")

            case BoolValue(value) =>
                if( this.valueType == "Undefined" ){
                    this.value = new BoolValue(value)
                    this.valueType  = "Bool"}
                if( this.valueType == "Bool" )
                    this.value = new BoolValue(value)
                else throw new oberon.InvalidArgument("Assign: Invalid Type")

            case Undefined() => 
                if( this.valueType == "Undefined" )
                    this.value = new Undefined()
                else throw new oberon.InvalidArgument("Assign: Invalid Type")
            
            case _ => throw new  oberon.InvalidArgument("Assign: Unexpected Type")
        }
        return this
    }
    
    def evaluate  (that: Value): Boolean = this.evaluate( (new Variable)(that) ) 

    def evaluate  (that: Variable): Boolean = if(this.valueType == "Undefined") true else this.valueType == that.valueType

    def compare   (that: Variable): Int = {
        if( !this.evaluate(that) ) return -1
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
            case _ => throw new oberon.InvalidArgument("Compare: Unexpected Type")
        }
    }

    def equalTo   (that: Variable): Boolean = if( this.compareTo(that) == 0 ) true else false
}