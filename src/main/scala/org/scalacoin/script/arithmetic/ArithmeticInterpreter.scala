package org.scalacoin.script.arithmetic

import org.scalacoin.script.{ScriptProgramImpl, ScriptProgram}
import org.scalacoin.script.constant.{ScriptNumber, ScriptNumberImpl, ScriptConstantImpl, ScriptToken}

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
