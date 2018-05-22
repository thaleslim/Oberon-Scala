package oberon

import scala.collection.mutable.Stack
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

import oberon.expression.Value
import oberon.expression.Variable
import oberon.expression.Expression

import oberon.command.Command
import oberon.command.Procedure

object Environment {
  // A program is a stack of functions scopes 
  // and every function has its internal scopes
  // that have its own set of variables
  var program   = new Stack[Stack[Map[String, Variable]]]()
  var global:    Map[String,Variable]        = new HashMap[String, Variable]()
  var functions: Map[String,Procedure]       = new HashMap[String, Procedure]()

  // Gets the current scope
  def current() = {
    if( program.isEmpty )
        program.push(new Stack[Map[String, Variable]])
    program.top
  }

  def push() {
    current.push(new HashMap[String, Variable]())
  }

  def pop() {
    current.pop()
  }

  def map(id: String, value: Value) {
    if( program.isEmpty ) //Global Variable
        global += ( id -> (new Variable)(value) )
    else{
        if( current.isEmpty )
            push()
        
        if( exist(id) ) {
            if( !global.isEmpty ) global.get(id) match {
                case Some(thing) => {
                    var that = (new Variable)(value)
                    if( thing.evaluate(that) )
                        global += ( id -> that )
                    else throw new oberon.InvalidArgument("Assignment: Different types between declaration and argument")}
                case None => update(id,value) }
            else update(id,value) }
        else current.top += (id -> (new Variable)(value) )
    }
  }

  private def update(id: String, value: Value): Unit = {
    var it = current.iterator
    var map: Map[String,Variable] = null
    it.foreach{ target: Map[String, Variable] => if( map == null && target.get(id) != None ){ map = target }}
    if( map != null ) map.get(id) match{
        case Some(variable) => { 
            var that = (new Variable)(value);
            if( variable.evaluate(that) )
                map += ( id -> (new Variable)(value) )
            else throw new oberon.InvalidArgument("Assignment: Different types between declaration and argument")}
        case None => {}
    }
  }

  //Escopo Global tem prioridade ao escopo mais interno 
  def lookup(id: String, force: Boolean = true) : Option[Value] = {
    if( ( program.isEmpty || current.isEmpty ) && global.isEmpty )
        None
    else
        global.get(id) match {
            case None => if( program.isEmpty || current.isEmpty ) None
                else current.top.get(id) match {
                case None => {
                    if(!force) None
                    var it = current.iterator
                    var res: Option[Variable] = None
                    it.foreach{ map: Map[String, Variable] => if(res == None){res = map.get(id)}}
                    res match {
                        case Some(variable) => Some(variable.value)
                        case None => None
                    }
                }
                case Some(variable) => Some(variable.value)
            }
            case Some(variable) => Some(variable.value)
        }
  }

  def exist(id: String) : Boolean = lookup(id,false) match {case Some(v) => true; case None => false}

  def clear() : Unit = { program.clear(); global.clear(); functions.clear() }
}
