package org.scalacoin.script

import org.scalacoin.protocol.transaction.Transaction
import org.scalacoin.script.constant.ScriptToken

/**
 * Created by chris on 2/3/16.
 */
trait ScriptProgram {
  def stack : List[ScriptToken]
  def script : List[ScriptToken]
  def transaction : Transaction
  def altStack : List[ScriptToken]
  def valid : Boolean
}

case class ScriptProgramImpl(stack : List[ScriptToken],script : List[ScriptToken], transaction : Transaction,
                              altStack : List[ScriptToken] = List(), override val valid : Boolean = true) extends ScriptProgram


