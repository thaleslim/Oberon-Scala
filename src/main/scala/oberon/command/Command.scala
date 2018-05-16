package oberon.command

import oberon.Environment._

import oberon.expression.Expression
import oberon.expression.BoolValue
import oberon.expression.Undefined

trait Command {
  def run() : Unit 
}

class BlockCommand(val cmds: List[Command]) extends Command {
  override
  def run() : Unit = {
    cmds.foreach(c => c.run())
  }
}

// TODO: Ask teacher about declaration method and it's relation to the Map
class Declaration(val id: String) extends Command {
  override
  def run() : Unit = {
    var variable = new Assignment(id, new Undefined())
    variable.run
  }
}

class Assignment(val id: String, val expression: Expression) extends Command {

  override
  def run() : Unit = {
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
// TODO: Implement readInt(), readBool()
// TODO: Ask proffessor about read functions

//    for(command;cond;command)
class For(var previous: Command, cond: Expression, command: Command) extends While(cond,command) {
  override
  def run() : Unit = {
    // Creates a clone of the current scope' environment variables
    var before = stack.top.clone

    // Run all this scope' commands
    previous.run

    val whileCom = new While(cond,command)
    // Run the Blockcomand as many times as the conditions allows
    whileCom.run

    // Verifies if the variables mappings existed before this command' execution
    //        in order to remove this variables from the main scope
    stack.push(stack.pop.filter{ p: Tuple2[String,oberon.expression.Value] => if( before.get(p._1) == None ) false else true })
  }
}
/**
    If(cond) Then command
  */
class IfThen(val cond: Expression, val command: Command) extends Command {
  override
  def run() : Unit = {
    // println("Condicao:" + cond)
    // println("Environment: ")
    // println(stack)

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
    // println("Condicao:" + cond)
    // println("Environment: ")
    // println(stack)

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
    print(exp.eval())
  }

}

class Procedure(val id: String, val commands: BlockCommand) extends Command{
    override
    def run() : Unit = {
        push()
        commands.run()
        pop()
    }
}
