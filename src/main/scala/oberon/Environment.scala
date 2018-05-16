package oberon

import scala.collection.mutable.Stack
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

import oberon.expression.Value
import oberon.expression.Expression

object Environment {
  var stack = new Stack[Map[String, Value]] () 

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
  def lookup(id: String) : Option[Value] = stack.top.get(id)

  def clear() : Unit = { stack.clear() } 
}
