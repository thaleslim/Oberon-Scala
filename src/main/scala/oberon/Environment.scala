package oberon

import scala.collection.mutable.Stack
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

import oberon.expression.Value
import oberon.expression.Expression

object Environment {
  // TODO: Tuple2 armazenando tipo e valor
  // ReadInt ReadBool === scanf predef
  var stack  = new Stack[Map[String, Value]]()

  def push() {
    stack.push(new HashMap[String, Value]())
  }

  def pop() {
    stack.pop()
  }

  def map(id: String, value: Value) {
    if(stack.isEmpty) {
      push()
    }
    stack.top += (id -> value)
  }

  // Previous solution generated a Exception whenever you'd request for a non-existent id
  // TODO: caso não encontre no escopo atual, buscar nos outros escopos (Iterator)
  def lookup(id: String) : Option[Value] = {
    if(stack.isEmpty)
        None 
    else 
        stack.top.get(id) // match { 
            // case None => Somente se não encontrar em nenhum escopo
        // }
  }

  def clear() : Unit = { stack.clear() } 
}
