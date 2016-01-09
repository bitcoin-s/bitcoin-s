package org.scalacoin.script.bitwise

import org.scalacoin.script.constant.{ScriptConstantImpl, ScriptToken}
import org.scalacoin.script.control.{OP_VERIFY, ControlOperationsInterpreter}

/**
 * Created by chris on 1/6/16.
 */
trait BitwiseInterpreter extends ControlOperationsInterpreter  {

  /**
   * 	Returns 1 if the inputs are exactly equal, 0 otherwise.
   * @param stack
   * @param script
   * @return
   */
  def equal(stack : List[ScriptToken], script : List[ScriptToken]) : (List[ScriptToken], List[ScriptToken]) = {
    require(stack.size > 1, "Stack size must be 2 or more to compare the top two values")
    require(script.headOption.isDefined && script.head == OP_EQUAL, "Script operation must be OP_EQUAL")
    val newStack = stack match {
      case h :: h1 :: t => if (h == h1) ScriptConstantImpl("1") :: t else ScriptConstantImpl("0") :: t
    }
    (newStack,script.tail)

  }

  /**
   * Same as OP_EQUAL, but runs OP_VERIFY afterward.
   * @param stack
   * @param script
   * @return
   */
  def equalVerify(stack : List[ScriptToken], script : List[ScriptToken]) : Boolean = {
    require(stack.size > 1, "Stack size must be 2 or more to compare the top two values")
    require(script.headOption.isDefined && script.head == OP_EQUALVERIFY, "Script operation must be OP_EQUALVERIFY")
    //first replace OP_EQUALVERIFY with OP_EQUAL and OP_VERIFY
    val simpleScript = OP_EQUAL :: OP_VERIFY :: script.tail
    val (newStack,newScript) = equal(stack,simpleScript)
    val result : Boolean = verify(newStack,newScript)
    result
  }


}
