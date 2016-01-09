package org.scalacoin.protocol.script

import org.scalacoin.script.constant.ScriptToken

/**
 * Created by chris on 12/26/15.
 */
trait ScriptSignature {
  def asm : Seq[ScriptToken]
  def hex : String
}

case class ScriptSignatureImpl(asm : Seq[ScriptToken], hex : String) extends ScriptSignature