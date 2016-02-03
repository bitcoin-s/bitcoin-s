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
   * @param stack
   * @param script
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
   * Puts the number of stack items onto the stack.
   * @param stack
   * @param script
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

}
