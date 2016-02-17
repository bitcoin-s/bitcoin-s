package org.scalacoin.protocol.script

import org.scalacoin.marshallers.transaction.TransactionElement
import org.scalacoin.script.constant.{OP_0, ScriptToken}
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

  def signature : ScriptToken  = {
    if (asm.headOption.isDefined && asm.head == OP_0) {
      //must be p2sh because of bug that forces p2sh scripts
      //to begin with OP_0
      asm(2)
    } else asm(1)
  }
  def hashType : HashType = {
    require(HashTypeFactory.fromByte(signature.bytes.last).isDefined,
      "Hash type could not be read for this scriptSig: " + signature.hex)
    HashTypeFactory.fromByte(signature.bytes.last).get
  }

}

case class ScriptSignatureImpl(asm : Seq[ScriptToken], hex : String) extends ScriptSignature