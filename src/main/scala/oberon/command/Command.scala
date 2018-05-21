package oberon.command

import scala.collection.mutable.Stack
import scala.collection.mutable.Map

import oberon.Environment._

import oberon.expression.Value
import oberon.expression.Expression
import oberon.expression.IntValue
import oberon.expression.BoolValue
import oberon.expression.Undefined
import oberon.expression.Variable

trait Command {
  def run() : Unit 
}

class BlockCommand(val cmds: List[Command]) extends Command {
  override
  def run() : Unit = {
    cmds.foreach(c => c.run())
  }
}
// TODO: improve link var <-> type // TODO: fix link
class Declaration(val id: String, val dataType: Value) extends Command {
  override
  def run() : Unit = {
    if( exist(id) ) throw new oberon.InvalidArgument("Double Declaration")
    map(id, dataType)
   
//   def map(id: String, value: Value) {
//     if( program.isEmpty ) //Global Variable
//         global += ( id -> (new Variable)(value) )
//     else{
//         if( current.isEmpty )
//             push()
        
//         if( exist(id) ) {
//             if( !global.isEmpty ) global.get(id) match {
//                 case Some(thing) => global += ( id -> (new Variable)(value) )
//                 case None => update(id,value) }
//             else update(id,value) }
//         else current.top += (id -> (new Variable)(value) )
//     }
//   }
  }
}

class Assignment(val id: String, val expression: Expression) extends Command {

  override
  def run() : Unit = {
    //if( !exist(id) ) throw new oberon.InvalidArgument("Undeclared Variable")
    map(id, expression.eval())
  }

}

class While(val cond: Expression, val command: Command) extends Command {
  override
  def run() : Unit = {
    // println("Condicao:" + cond)
    // println("Environment: ")
    // println(stack)

    val v = cond.eval.asInstanceOf[BoolValue]

    v match {
      case BoolValue(true) => { command.run(); this.run(); }
      case _               => { } 
    }
  }
}

//    for(command;cond;command)
class For(var previous: Command, cond: Expression, command: Command) extends While(cond,command) {
  override
  def run() : Unit = {
    //Creates a new scope inside the main function' scope
    push()

    // Run all this scope' commands
    previous.run

    val whileCom = new While(cond,command)
    // Run the Blockcomand as many times as the conditions allows
    whileCom.run

    //Clear the scope used
    pop()
  }
}
/**
    If(cond) Then command
  */
class IfThen(val cond: Expression, val command: Command) extends Command {
  override
  def run() : Unit = {

    val v = cond.eval.asInstanceOf[BoolValue]

    v match {
      case BoolValue(true) => { command.run() }
      case _               => {} 
    }
  }
}
/**
    If(cond) Then commandTrue
    else          commandFalse
  */
class IfThenElse( cond: Expression, commandTrue: Command, val commandFalse: Command) extends IfThen(cond,commandTrue) {
  override
  def run() : Unit = {

    val v = cond.eval.asInstanceOf[BoolValue]

    v match {
      case BoolValue(true) => { commandTrue.run()  }
      case BoolValue(false)=> { commandFalse.run() } 
    }
  }
}

class Print(val exp: Expression) extends Command {
  override
  def run() : Unit = {
    exp.eval() match {
        case IntValue(value) => print(value)
        case BoolValue(value) => print(value)
        case Undefined()       => print("")
        case _ => throw new oberon.InvalidArgument("oops a error occurred: Unexpected Value for print()")
    }
  }

}

// procedure id( (id,value)* ) commands
class Procedure(val commands: BlockCommand, val param: Tuple2[String,Variable]*){
    def declare(id: String){
        functions += (id -> this)
    }
    
    private def tryout(passed: Integer, that: Tuple2[String,Variable]* ): Boolean = {
        if( !that.isEmpty ) {
            var temp: Tuple2[String,Variable] = null
            if( param.find{tuple: Tuple2[String,Variable] => if(that.head._2.evaluate(tuple._2)){ temp = tuple; true } else false } == None
             && param.indexOf(temp) != (that.indexOf(that.head) + passed) )
                return false
            return this.tryout(passed + 1, that.tail: _*)
        }
        return true
    }

    def check(args: Tuple2[String,Variable]* ): Boolean = 
        if( ( args.isEmpty && param.isEmpty ) || ( !args.isEmpty && !param.isEmpty ) )
            return this.tryout(0, args: _*)
        else
            return false

    // TODO: Gabriel Santo para trote 2018.2

    def loadargs(that: Tuple2[String,Variable]*) : Unit = {
        var temp = param.toList
        var index: Integer = 0
        temp.foreach{ tuple: Tuple2[String,Variable] => map( tuple._1, new Undefined() ) }
        that.toList.foreach{ tuple: Tuple2[String,Variable] => { map( temp(index)._1, tuple._2.eval() ); index += 1 } }
    }
}

class ProcedureCall(val id: String, val param: Tuple2[String,Variable]*) extends Command {
    override
    def run() : Unit = {
        functions.get(id) match {
            case None => throw new oberon.InvalidArgument("Call to undeclared procedure")
            case Some(procedure) => {
                if( procedure.check(this.param: _*) )
                    this.execute(procedure)
                else throw new oberon.InvalidArgument("Invalid Argument sequence, according to procedure declaration")
            }
        }
    }

    // private def loadargs(that: Tuple2[String,Variable]*) : Unit = {
    //     if(!that.isEmpty){
    //         map( that.head._1, that.head._2.eval() )
    //         this.loadargs(that.tail: _*)
    //     }
    // }

    private def execute(procedure: Procedure) : Unit = {
        program.push(new Stack[Map[String, Variable]])
        
        procedure.loadargs(this.param: _*)

        procedure.commands.run

        program.pop()
    }
}
