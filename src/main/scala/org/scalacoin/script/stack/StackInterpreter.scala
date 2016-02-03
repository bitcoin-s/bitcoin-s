package org.scalacoin.script.stack

import org.scalacoin.script.{ScriptProgramImpl, ScriptProgram}
import org.scalacoin.script.constant._

/**
 * Created by chris on 1/6/16.
 * Stack operations implemented in the script programming language
 * https://en.bitcoin.it/wiki/Script#Stack
 */
trait StackInterpreter {

  /**
   * Duplicates the element on top of the stack
   * expects the first element in script to be the OP_DUP operation
   * @param program
   * @return
   */
  def opDup(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_DUP, "Top of the script stack must be OP_DUP")
    require(program.stack.headOption.isDefined, "Cannot duplicate the top element on an empty stack")
    program.stack match {
      case h :: t => ScriptProgramImpl(h :: program.stack, program.script.tail,program.transaction,program.altStack)
      case Nil => throw new RuntimeException("Received an empty stack! Cannot duplicate an element on an empty stack")
    }
  }

  /**
   * If the top stack value is not 0, duplicate it.
   * @param program
   * @return
   */
  def opIfDup(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_IFDUP, "Top of the script stack must be OP_DUP")
    require(program.stack.headOption.isDefined, "Cannot duplicate the top element on an empty stack")
    if (program.stack.head == OP_0) {
      ScriptProgramImpl(program.stack,program.script.tail, program.transaction,program.altStack)
    } else ScriptProgramImpl(program.stack.head :: program.stack,
      program.script.tail, program.transaction,program.altStack)
  }

  /**
   * Puts the number of stack items onto the stack.
   * @param program
   * @return
   */
  def opDepth(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_DEPTH, "Top of script stack must be OP_DEPTH")
    require(program.script.size >= 1, "OP_DEPTH requires at least two elements on the script stack")
    val operation = program.script.head
    val stackSize = program.stack.size

    val numberToPush : Option[ScriptNumber] = ScriptNumberFactory.factory(stackSize)
    require(numberToPush.isDefined, "Stack size was to large to find in the script number factory, stack size was: " + stackSize)
    ScriptProgramImpl(numberToPush.get :: program.stack, program.script.tail,program.transaction,program.altStack)
  }

  /**
   * Puts the input onto the top of the alt stack. Removes it from the main stack.
   * @param program
   * @return
   */
  def opToAltStack(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_TOALTSTACK, "Top of script stack must be OP_TOALTSTACK")
    require(program.stack.size > 0,"Stack must have at least one item on it for OP_TOALTSTACK")
    ScriptProgramImpl(program.stack.tail,program.script.tail,program.transaction,List(program.stack.head))
  }

  /**
   * Puts the input onto the top of the main stack. Removes it from the alt stack.
   * @param program
   * @return
   */
  def opFromAltStack(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_FROMALTSTACK, "Top of script stack must be OP_FROMALTSTACK")
    require(program.altStack.size > 0,"Alt Stack must have at least one item on it for OP_FROMALTSTACK")
    ScriptProgramImpl(program.altStack.head :: program.stack,
      program.script.tail, program.transaction,program.altStack.tail)
  }

  /**
   * Removes the top stack item.
   * @param program
   * @return
   */
  def opDrop(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_DROP, "Top of script stack must be OP_DROP")
    require(program.stack.size > 0,"Stack must have at least one item on it for OP_DROP")
    ScriptProgramImpl(program.stack.tail,program.script.tail,program.transaction,program.altStack)
  }


  /**
   * Removes the second-to-top stack item
   * @param program
   * @return
   */
  def opNip(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_NIP, "Top of script stack must be OP_NIP")
    require(program.stack.size > 1,"Stack must have at least two items on it for OP_NIP")
    program.stack match {
      case h :: h1 :: t => ScriptProgramImpl(h :: t, program.script.tail, program.transaction, program.altStack)
    }
  }

}
