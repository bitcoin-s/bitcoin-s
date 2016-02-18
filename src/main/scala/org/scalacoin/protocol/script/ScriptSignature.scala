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

  /**
   * Derives the hash type for a given scriptSig
   * @param scriptSig
   * @return
   */
  def hashType(scriptSig : Seq[Byte]) : HashType = {
    require(HashTypeFactory.fromByte(scriptSig.last).isDefined,
      "Hash type could not be read for this scriptSig: " + ScalacoinUtil.encodeHex(scriptSig))
    HashTypeFactory.fromByte(scriptSig.last).get
  }

  /**
   * Derives the hash type for a give scriptSig
   * @param scriptSigHex
   * @return
   */
  def hashType(scriptSigHex : String) : HashType = hashType(ScalacoinUtil.decodeHex(scriptSigHex))

}

case class ScriptSignatureImpl(asm : Seq[ScriptToken], hex : String) extends ScriptSignature