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
// TODO: Implement FOR, print(), readInt(), readBool()
// TODO: Ask proffessor about read functions
 

class For(val cont: Command ,cond: Expression, val fcommand: Command, val com: Command) extends While(cond,com) {
  override
  def run() : Unit = {
    cont.run()
    val w1 = new While(cond, new BlockCommand(List(com,fcommand)))
    w1.run()
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
