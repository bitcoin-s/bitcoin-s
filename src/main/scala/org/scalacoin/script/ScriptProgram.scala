package org.scalacoin.script

import org.scalacoin.protocol.transaction.Transaction
import org.scalacoin.script.constant.{OP_0, ScriptNumberImpl, ScriptFalse, ScriptToken}

/**
 * Created by chris on 2/3/16.
 */
trait ScriptProgram {
  def stack : List[ScriptToken]
  def script : List[ScriptToken]
  def transaction : Transaction
  def altStack : List[ScriptToken]
  def valid : Boolean

  /**
   * Returns if the stack top is true
   * @return
   */
  def stackTopIsTrue = !stackTopIsFalse

  /**
   * Returns if the stack top is false
   * @return
   */
  def stackTopIsFalse : Boolean = {
    if (stack.headOption.isDefined &&
      (stack.head == ScriptFalse || stack.head == ScriptNumberImpl(0) || stack.head == OP_0)) true
    else if (!stack.headOption.isDefined) true
    else false
  }
}

case class ScriptProgramImpl(stack : List[ScriptToken],script : List[ScriptToken], transaction : Transaction,
                              altStack : List[ScriptToken], override val valid : Boolean = true) extends ScriptProgram


