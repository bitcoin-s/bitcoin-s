package org.scalacoin.protocol.script

import org.scalacoin.crypto.{ECFactory, ECDigitalSignature}
import org.scalacoin.marshallers.transaction.TransactionElement

import org.scalacoin.script.constant._
import org.scalacoin.script.crypto.{OP_CHECKMULTISIG, HashType, HashTypeFactory}
import org.scalacoin.util.{BitcoinSUtil}
import org.slf4j.LoggerFactory

/**
 * Created by chris on 12/26/15.
 */
sealed trait ScriptSignature extends TransactionElement {

  private def logger = LoggerFactory.getLogger(this.getClass())

  /**
   * Representation of a scriptSignature in a parsed assembly format
   * this data structure can be run through the script interpreter to
   * see if a script evaluates to true
   * @return
   */
  def asm : Seq[ScriptToken]

  def hex : String
  /**
   * The digital signatures contained inside of the script signature
   * p2pkh script signatures only have one sig
   * p2sh script signatures can have n sigs
   * @return
   */
  def signatures : Seq[ECDigitalSignature]  = {
    if (asm.headOption.isDefined && asm.head == OP_0) {
      //must be p2sh because of bug that forces p2sh scripts
      //to begin with OP_0
      //scripSig for p2sh input script
      //OP_0 <scriptSig> <scriptSig> ... <scriptSig> <redeemScript>
      val scriptSigs = asm.slice(1,asm.size-1)
      //filter out all of the PUSHDATA / BytesToPushOntoStack operations
      val scriptSigsWithoutPushOps = scriptSigs.filterNot(op => op.isInstanceOf[BytesToPushOntoStack]
        || op == OP_PUSHDATA1
        || op == OP_PUSHDATA2
        || op == OP_PUSHDATA4)
      scriptSigsWithoutPushOps.map(sig => ECFactory.digitalSignature(sig.bytes))
    } else Seq(ECFactory.digitalSignature(asm(1).bytes))
  }

  /**
   * Derives the hash type for a given digitalSignature
   * @param digitalSignature
   * @return
   */
  def hashType(digitalSignature: ECDigitalSignature) = {
    require(HashTypeFactory.fromByte(digitalSignature.bytes.last).isDefined,
      "Hash type could not be read for this scriptSig: " + digitalSignature.hex)
    HashTypeFactory.fromByte(digitalSignature.bytes.last).get
  }
}

case class ScriptSignatureImpl(asm : Seq[ScriptToken], hex : String) extends ScriptSignature