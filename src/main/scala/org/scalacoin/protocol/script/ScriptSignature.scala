package org.scalacoin.protocol.script

import org.scalacoin.marshallers.transaction.TransactionElement
import org.scalacoin.script.constant._
import org.scalacoin.script.crypto.{OP_CHECKMULTISIG, HashType, HashTypeFactory}
import org.scalacoin.util.ScalacoinUtil
import org.slf4j.LoggerFactory

/**
 * Created by chris on 12/26/15.
 */
trait ScriptSignature extends TransactionElement {

  private def logger = LoggerFactory.getLogger(this.getClass())
  def asm : Seq[ScriptToken]
  def hex : String

  def signatures : Seq[ScriptToken]  = {
    if (asm.headOption.isDefined && asm.head == OP_0) {
      //must be p2sh because of bug that forces p2sh scripts
      //to begin with OP_0
      //scripSig for p2sh input script
      //OP_0 <scriptSig> <scriptSig> ... <scriptSig> <redeemScript>
      val scriptSigs = asm.slice(1,asm.size-1)
      //filter out all of the PUSHDATA / BytesToPushOntoStack operations
      scriptSigs.filterNot(op => op.isInstanceOf[BytesToPushOntoStack]
        || op == OP_PUSHDATA1
        || op == OP_PUSHDATA2
        || op == OP_PUSHDATA4)
    } else Seq(asm(1))
  }
  def hashType : HashType = {
    require(HashTypeFactory.fromByte(signatures.head.bytes.last).isDefined,
      "Hash type could not be read for this scriptSig: " + signatures.head.hex)
    HashTypeFactory.fromByte(signatures.head.bytes.last).get
  }

}

case class ScriptSignatureImpl(asm : Seq[ScriptToken], hex : String) extends ScriptSignature