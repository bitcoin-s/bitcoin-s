package org.scalacoin.protocol.script

import org.scalacoin.marshallers.transaction.TransactionElement
import org.scalacoin.script.constant.ScriptToken
import org.scalacoin.script.crypto.{HashType, HashTypeFactory}
import org.scalacoin.util.ScalacoinUtil
import org.slf4j.LoggerFactory

/**
 * Created by chris on 12/26/15.
 */
trait ScriptSignature extends TransactionElement {

  private def logger = LoggerFactory.getLogger(this.getClass())
  def asm : Seq[ScriptToken]
  def hex : String

  def signature : Seq[ScriptToken]  = Seq(asm(1))
  def hashType : HashType = {
    require(HashTypeFactory.fromByte(signature.head.bytes.last).isDefined,
      "Hash type could not be read for this scriptSig: " + signature.head.hex)
    HashTypeFactory.fromByte(signature.head.bytes.last).get
  }

}

case class ScriptSignatureImpl(asm : Seq[ScriptToken], hex : String) extends ScriptSignature