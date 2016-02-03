package org.bitcoins.protocol.script

/**
 * Created by chris on 12/26/15.
 */
trait ScriptSignature {
  def asm : String
  def hex : String
}

case class ScriptSignatureImpl(asm : String, hex : String) extends ScriptSignature