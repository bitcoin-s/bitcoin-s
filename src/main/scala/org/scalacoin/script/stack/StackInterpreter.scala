package org.scalacoin.script.stack

import org.scalacoin.script.constant.ScriptToken

/**
 * Created by chris on 1/6/16.
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
}
