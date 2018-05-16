package oberon.command

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.GivenWhenThen
import org.scalatest.BeforeAndAfter


import oberon.Environment._

import oberon.expression.IntValue
import oberon.expression._
import oberon.command._

class TestFor extends FlatSpec with Matchers with GivenWhenThen with BeforeAndAfter {

  behavior of "a for command"

  before {
    clear()
  }
  // x;
  // soma = 0;
  // for(x = 1;x < 11; x++)
  //   soma := soma + x;
  // end
  // print(soma);  
  it should "lookup(soma) must be equal to 55 after a loop summing up 1 to 10" in {
    val d1 = new Declaration("x") // x;
    val a1 = new Assignment("soma", IntValue(0))     // soma := 0;
    val a2 = new Assignment("soma",new AddExpression(new VarRef("soma"), new VarRef("x")))
    val a3 = new Assignment("x", IntValue(1))
    val a4 = new Assignment("x",new AddExpression(new VarRef("x"),IntValue(1)))
    val cond = new SlExpression(new VarRef("x"), IntValue(11))
    val f1 = new For(new BlockCommand(List(d1,a3)),cond, new BlockCommand(List(a4)), new BlockCommand(List(a2)))

    a1.run()
    f1.run()

    val res = lookup("soma")
    res match {
      case Some(v) => v.eval() should be (IntValue(55))
      case _       => 5 should be (1)  
    }
  }
  
  it should "show x=10 after a for(x=0;x<10;x++)" in{
    val d1 = new Declaration("x") //x;
    val a1 = new Assignment("x",IntValue(0)) //x=0
    val a2 = new Assignment("x",new AddExpression(new VarRef("x"),IntValue(1)))
    val c1 = new SlExpression(new VarRef("x"),IntValue(10))
    val f1 = new For(new BlockCommand(List(d1,a1)),c1,new BlockCommand(List(a2)), new BlockCommand(List()))
    
    f1.run()
    
    val y = lookup("x")
    y match{
        case Some(v) => v.eval() should be (IntValue(10))
        case _ => 0 should be (1)
    }
  }
  
  it should "show x=1 after a for(x=10;x>1;x--)" in{
  val d1 = new Declaration("x") 
  val a1 = new Assignment ("x",IntValue(10))
  val a2 = new Assignment ("x", new SubExpression(new VarRef("x"),IntValue(1)))
  val c1 = new SgExpression(new VarRef("x"),IntValue(1))
  val f1 = new For(new BlockCommand(List(d1,a1)),c1,new BlockCommand(List(a2)),new BlockCommand(List()))
  
  f1.run()
  
  val m = lookup("x")
  m match{
    case Some(v) => v.eval() should be (IntValue(1))
    case _ => 0 should be (1)
  }
  
  }
}

