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

/** A command representation.
  
  * This should, by definition, be able to run properly.
  */
trait Command {
  def run() : Unit 
}

// TODO: Use a Seq instead of a List
/** begin [...] end */
class BlockCommand(val cmds: List[Command]) extends Command {
  override
  def run() : Unit = {
    cmds.foreach(c => c.run())
  }
}

// TODO: Declaration could receive a String as a arg instead of a Value
/** id: type */
class Declaration(val id: String, val dataType: Value) extends Command {
  override
  def run() : Unit = {
    if( exist(id) ) throw new oberon.InvalidArgument("Double Declaration")
    var dataName: String = "Undefined"

    dataType.eval() match {
        case IntValue(value)  => dataName = "Int"
        case BoolValue(value) => dataName = "Bool"
        case Undefined()      => throw new oberon.InvalidArgument("Declaration: Invalid Variable Type")
        case _ => throw new oberon.InvalidArgument("Declaration: oops unexpected")
    }
    
    if( program.isEmpty )
        global += ( id -> new Variable(dataName) )
    else{
        if( current.isEmpty )
            push()
        current.top += (id -> new Variable(dataName) )
    }
  }
}

/** id := Value || Expression */
class Assignment(val id: String, val expression: Expression) extends Command {

  override
  def run() : Unit = {
    //if( !exist(id) ) throw new oberon.InvalidArgument("Undeclared Variable")
    map(id, expression.eval())
  }

}

// TODO: Arguments should receive a CommanBlock ... or not?
/** while(cond) do begin [...] end || while(cond) do command */
class While(val cond: Expression, val command: Command) extends Command {
  override
  def run() : Unit = {

    val v = cond.eval.asInstanceOf[BoolValue]

    v match {
      case BoolValue(true) => { command.run(); this.run(); }
      case _               => { } 
    }
  }
}

// TODO: Same design decision as While: Command or CommandBlock
/* AskTeacher: Should the abstraction integrate the for declare commands along with it's commands or should it be split? 
    A.K.A for( command; cond; command) command || for( command; cond; command ) */
/** for( command; cond; command ) begin [...] end && for( command; cond; command) command */
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

// TODO: Same design decision as For: Command or CommandBlock
/** if(cond) then command || if(cond) then begin [...] end */
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

// TODO: Same design decision as IfThen: Command or CommandBlock
/** if(cond) then command else command
    or
    if(cond) then begin [...] end else begin [...] end
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

/** print( Expression )*/
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

// TODO: review hierarchy and relation of this with environment: does it know too much?
// procedure id( (id,value)* ) commands
/** procedure id( arguments ) begin [...] end */
class Procedure(val commands: BlockCommand, val param: Tuple2[String,Variable]*){
    def declare(id: String){
        functions += (id -> this)
    }
    
    private def tryout(passed: Integer, that: Expression* ): Boolean = {
        if( !that.isEmpty ) {
            var temp: Tuple2[String,Variable] = null
            if ( param.find {   tuple: Tuple2[String,Variable] =>
                                    if( tuple._2.evaluate( that.head.eval() ) ) {
                                        temp = tuple;
                                        true}
                                    else false
                            } == None &&
                param.indexOf(temp) != ( that.indexOf(that.head) + passed )
                )
                return false
            return this.tryout(passed + 1, that.tail: _*)
        }
        return true
    }

    def check(args: Expression* ): Boolean = 
        if( ( args.isEmpty && param.isEmpty ) || ( !args.isEmpty && !param.isEmpty ) )
            return this.tryout(0, args: _*)
        else
            return false

    // TODO: Gabriel Santo para trote 2018.2

    def loadargs(that: Expression*) : Unit = {
        var temp = param.toList
        var index: Integer = 0
        temp.foreach{ tuple: Tuple2[String,Variable] => map(tuple._1, new Undefined()) }
        that.toList.foreach{ value: Expression => { map( temp(index)._1, value.eval() ); index += 1 } }
    }
}

/** call id */
class ProcedureCall(val id: String, val param: Expression*) extends Command {
    override
    def run() : Unit = functions.get(id) match {
        case None => throw new oberon.InvalidArgument("Call to undeclared procedure")
        case Some(procedure) => {
            //param.toList.foreach{x: Expression => print(x.eval())}
            if( procedure.check(this.param: _*) )
                this.execute(procedure)
            else throw new oberon.InvalidArgument("Invalid Argument sequence, according to procedure declaration")
        }
    }

    private def execute(procedure: Procedure) : Unit = {
        program.push(new Stack[Map[String, Variable]])
        
        procedure.loadargs(this.param: _*)

        procedure.commands.run

        program.pop()
    }
}


// TODO: Declaração de função(Class) && Return(Command) && Chamada de Função(Expressão)