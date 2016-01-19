package org.scalacoin.script.stack

import org.scalacoin.script.constant.{ScriptConstantImpl, ScriptConstant, ScriptToken}

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
  def opDup(stack : List[ScriptToken], script : List[ScriptToken]) : (List[ScriptToken], List[ScriptToken]) = {
    require(script.headOption.isDefined && script.head == OP_DUP, "Top of the script stack must be OP_DUP")
    require(stack.headOption.isDefined, "Cannot duplicate the top element on an empty stack")
    stack match {
      case h :: t => (h :: stack, script.tail)
      case Nil => throw new RuntimeException("Received an empty stack! Cannot duplicate an element on an empty stack")
    }
  }

  /**
   * Puts the number of stack items onto the stack.
   * @param stack
   * @param script
   * @return
   */
  def opDepth(stack : List[ScriptToken], script : List[ScriptToken]) : (List[ScriptToken], List[ScriptToken]) = {
    require(script.headOption.isDefined && script.head == OP_DEPTH, "Top of script stack must be OP_DEPTH")
    require(script.size >= 1, "OP_DEPTH requires at least two elements on the script stack")
    val operation = script.head
    val numberToPush : ScriptConstant = ScriptConstantImpl(stack.size.toHexString)
    (numberToPush :: stack, script.tail)
  }

}
