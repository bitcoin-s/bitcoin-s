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

  def signature : String  = ???
  def hashType : HashType = {
    require(HashTypeFactory.fromByte(bytes.last).isDefined, "Hash type could not be read for this scriptSig: " + asm(1))
    HashTypeFactory.fromByte(asm(1).bytes.last).get
  }

}

case class ScriptSignatureImpl(asm : Seq[ScriptToken], hex : String) extends ScriptSignature