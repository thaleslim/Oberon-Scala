package oberon.expression

import oberon.Environment._

/** Retrieves the value from a variable from the current scope. */
class VarRef(val id: String) extends Expression {
    
    /** This id' value in the current scope. */
    override def eval() : Value = lookup(id) match {
        case Some(v) => v
        case _       => Undefined()   
    }

}
