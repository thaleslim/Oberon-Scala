package oberon

import scala.collection.mutable.Stack
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

import oberon.expression.Value
import oberon.expression.Variable
import oberon.expression.Expression

object Environment {
  var stack  = new Stack[Map[String, Variable]]()

  def push() {
    stack.push(new HashMap[String, Variable]())
  }

  def pop() {
    stack.pop()
  }

  def map(id: String, value: Value) {
    if(stack.isEmpty) {
      push()
    }
    stack.top += (id -> (new Variable)(value) )
  }

  // TODO: caso não encontre no escopo atual, buscar nos outros escopos (Iterator)
  def lookup(id: String, force: Boolean = true) : Option[Value] = {
    if(stack.isEmpty)
        None
    else
        stack.top.get(id) match {
            case None => None
            case Some(variable) => Some(variable.value)
        }
  }

  def clear() : Unit = { stack.clear() } 
}
