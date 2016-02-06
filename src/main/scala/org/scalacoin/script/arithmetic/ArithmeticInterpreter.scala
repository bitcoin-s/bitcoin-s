package org.scalacoin.script.arithmetic

import org.scalacoin.script.{ScriptProgramImpl, ScriptProgram}
import org.scalacoin.script.constant._

/**
 * Created by chris on 1/25/16.
 */
trait ArithmeticInterpreter {


  /**
   * a is added to b
   * @param program
   * @return
   */
  def opAdd(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_ADD, "Script top must be OP_ADD")
    require(program.stack.size > 1, "Stack size must be 2 or more perform an OP_ADD")

    val b  = numFromScriptToken(program.stack.head)
    val a = numFromScriptToken(program.stack(1))

    val result = numberToScriptToken(a + b)
    ScriptProgramImpl(result :: program.stack.slice(2,program.stack.size),
      program.script.tail, program.transaction, program.altStack)
  }

  /**
   * Increments the stack top by 1
   * @param program
   * @return
   */
  def op1Add(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_1ADD, "Script top must be OP_1ADD")
    require(program.stack.size > 0, "Stack size must be 1 or more perform an OP_1ADD")

    val newStackTop = program.stack.head match {
      case s : ScriptNumber => s + ScriptNumberImpl(1)
      case x => throw new RuntimeException("Stack must be script number to perform OP_1ADD, stack top was: " + x)
    }

    ScriptProgramImpl(newStackTop :: program.stack.tail,
      program.script.tail, program.transaction, program.altStack)
  }

  /**
   * Decrements the stack top by 1
   * @param program
   * @return
   */
  def op1Sub(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_1SUB, "Script top must be OP_1SUB")
    require(program.stack.size > 0, "Stack size must be 1 or more perform an OP_1SUB")

    val newStackTop = program.stack.head match {
      case s : ScriptNumber => s - ScriptNumberImpl(1)
      case x => throw new RuntimeException("Stack must be script number to perform OP_1ADD, stack top was: " + x)
    }

    ScriptProgramImpl(newStackTop :: program.stack.tail,
      program.script.tail, program.transaction, program.altStack)
  }


  /**
   * b is subtracted from a.
   * @param program
   * @return
   */
  def opSub(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_SUB, "Script top must be OP_SUB")
    require(program.stack.size > 1, "Stack size must be 2 or more perform an OP_SUB")

    val b = program.stack.head match {
      case s : ScriptNumber => s - ScriptNumberImpl(1)
      case x => throw new RuntimeException("Stack must be script number to perform OP_SUB, stack top was: " + x)
    }
    val a = program.stack.tail.head match {
      case s : ScriptNumber => s - ScriptNumberImpl(1)
      case x => throw new RuntimeException("Stack must be script number to perform OP_SUB, stack top was: " + x)
    }

    val newScriptNumber = a - b
    ScriptProgramImpl(newScriptNumber :: program.stack.tail,
      program.script.tail, program.transaction, program.altStack)
  }

  /**
   * Takes the absolute value of the stack top
   * @param program
   * @return
   */
  def opAbs(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_ABS, "Script top must be OP_ABS")
    require(program.stack.size > 0, "Stack size must be 1 or more perform an OP_ABS")
    val newStackTop = program.stack.head match {
      case s : ScriptNumber => ScriptNumberImpl(s.num.abs)
      case x => throw new RuntimeException("Stack must be script number to perform OP_ABS, stack top was: " + x)
    }
    ScriptProgramImpl(newStackTop :: program.stack.tail,
      program.script.tail, program.transaction, program.altStack)
  }

  /**
   * Negates the stack top
   * @param program
   * @return
   */
  def opNegate(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_NEGATE, "Script top must be OP_NEGATE")
    require(program.stack.size > 0, "Stack size must be 1 or more perform an OP_NEGATE")
    val newStackTop = program.stack.head match {
      case s : ScriptNumber => ScriptNumberImpl(-s.num)
      case x => throw new RuntimeException("Stack must be script number to perform OP_ABS, stack top was: " + x)
    }
    ScriptProgramImpl(newStackTop :: program.stack.tail,
      program.script.tail, program.transaction, program.altStack)
  }

  /**
   * If the input is 0 or 1, it is flipped. Otherwise the output will be 0.
   * @param program
   * @return
   */
  def opNot(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_NOT, "Script top must be OP_NOT")
    require(program.stack.size > 0, "Stack size must be 1 or more perform an OP_NOT")
    val newStackTop = program.stack.head match {
      case OP_0 => OP_1
      case OP_1 => OP_0
      case ScriptNumberImpl(0) => OP_1
      case ScriptNumberImpl(1) => OP_0
      case _ => OP_0
    }
    ScriptProgramImpl(newStackTop :: program.stack.tail,
      program.script.tail, program.transaction, program.altStack)
  }

  /**
   * Returns 0 if the input is 0. 1 otherwise.
   * @param program
   * @return
   */
  def op0NotEqual(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_0NOTEQUAL, "Script top must be OP_0NOTEQUAL")
    require(program.stack.size > 0, "Stack size must be 1 or more perform an OP_0NOTEQUAL")
    val newStackTop = program.stack.head match {
      case OP_0 => OP_0
      case ScriptNumberImpl(0) => OP_0
      case _ => OP_1
    }
    ScriptProgramImpl(newStackTop :: program.stack.tail,
      program.script.tail, program.transaction, program.altStack)
  }


  /**
   * 	If both a and b are not 0, the output is 1. Otherwise 0.
   * @param program
   * @return
   */
  def opBoolAnd(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_BOOLAND, "Script top must be OP_BOOLAND")
    require(program.stack.size > 1, "Stack size must be 2 or more perform an OP_BOOLAND")
    val b = program.stack.head
    val a = program.stack.tail.head
    val newStackTop = if ( b == a && (a == ScriptNumberImpl(0) || a == OP_0)) OP_1 else OP_0
    ScriptProgramImpl(newStackTop :: program.stack.tail,
      program.script.tail, program.transaction, program.altStack)

  }

  /**
   * If a or b is not 0, the output is 1. Otherwise 0.
   * @param program
   * @return
   */
  def opBoolOr(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_BOOLOR, "Script top must be OP_BOOLOR")
    require(program.stack.size > 1, "Stack size must be 2 or more perform an OP_BOOLOR")
    val b = program.stack.head
    val a = program.stack.tail.head
    val newStackTop = if (a == b && (a == ScriptNumberImpl(0) || a == OP_0)) OP_0 else OP_1
    ScriptProgramImpl(newStackTop :: program.stack.tail,
      program.script.tail, program.transaction, program.altStack)
  }
  /**
   * Wraps a scala number into a script token for the script language
   * @param num
   * @return
   */
  private def numberToScriptToken(num : Long) : ScriptToken = {
    ScriptNumberImpl(num)
  }


  /**
   * Converts a script token to an integer
   * @param token
   * @return
   */
  private def numFromScriptToken(token : ScriptToken) : Long = token match {
    case x : ScriptNumber => x.num
    case x : ScriptConstantImpl => Integer.parseInt(x.hex,16)
  }

}
