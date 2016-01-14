package org.scalacoin.protocol.script

import org.scalacoin.marshallers.transaction.TransactionElement
import org.scalacoin.script.constant.ScriptToken
import org.scalacoin.util.ScalacoinUtil

/**
 * Created by chris on 12/26/15.
 */
trait ScriptSignature extends TransactionElement {
  def asm : Seq[ScriptToken]
  def hex : String

  /**
    * Returns the size of the ScriptSignature in bytes
    * @return
  */
  override def size : Int = ScalacoinUtil.decodeHex(hex).size


}

case class ScriptSignatureImpl(asm : Seq[ScriptToken], hex : String) extends ScriptSignature