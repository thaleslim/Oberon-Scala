package oberon

import scala.collection.mutable.Stack
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

import oberon.expression.Value
import oberon.expression.Variable
import oberon.expression.Expression

import oberon.command.Command
import oberon.command.BlockCommand

object Environment {
  // A program is a stack of functions scopes 
  // and every function has its internal scopes
  // that have its own set of variables
  var program   = new Stack[Stack[Map[String, Variable]]]()
  var global:    Map[String,Variable]        = new HashMap[String, Variable]()
  var functions: Map[String,BlockCommand]    = new HashMap[String, BlockCommand]()

  // Gets the current scope
  def current() = {
    if(program.isEmpty)
        program.push(new Stack[Map[String, Variable]])
    program.top
  }

  def stack() = current()

  def push() {
    current.push(new HashMap[String, Variable]())
  }

  def pop() {
    current.pop()
  }

  def map(id: String, value: Value) {
    if(current.isEmpty)
      push()
    
    current.top += (id -> (new Variable)(value) )
  }

  // TODO: caso não encontre no escopo atual, buscar nos outros escopos (Iterator)
  def lookup(id: String) : Option[Value] = {
    if(program.isEmpty || current.isEmpty)
        None
    else
        global.get(id) match {
            case None => current.top.get(id) match {
                case None => None
                // Search inside current for id
                //{
                //
                //}
                case Some(variable) => Some(variable.value)
            }
            case Some(variable) => Some(variable.value)
        }
  }

  def clear() : Unit = { program.clear() }
}
