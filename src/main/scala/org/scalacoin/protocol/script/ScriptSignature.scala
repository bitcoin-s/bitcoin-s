package org.scalacoin.protocol.script

import org.scalacoin.crypto.{ECPublicKey, ECFactory, ECDigitalSignature}
import org.scalacoin.marshallers.script.ScriptParser
import org.scalacoin.marshallers.transaction.TransactionElement

import org.scalacoin.script.constant._
import org.scalacoin.script.crypto.{OP_CHECKMULTISIG, HashType, HashTypeFactory}
import org.scalacoin.util.{BitcoinSUtil}
import org.slf4j.LoggerFactory

//TODO: Need to add a scriptPubKey field to this script signature
//this corresponds to the output script that this scriptSignature is input for
/**
 * Created by chris on 12/26/15.
 *
 */
sealed trait ScriptSignature extends TransactionElement {

  protected def logger = LoggerFactory.getLogger(this.getClass())

  /**
   * Representation of a scriptSignature in a parsed assembly format
   * this data structure can be run through the script interpreter to
   * see if a script evaluates to true
   * @return
   */
  def asm : Seq[ScriptToken] = ScriptParser.fromBytes(bytes)

  /**
   * The digital signatures contained inside of the script signature
   * p2pkh script signatures only have one sig
   * p2sh script signatures can have m sigs
   * multisignature scripts can have m sigs
   * @return
   */
  def signatures : Seq[ECDigitalSignature]/*  = {
    if (asm.headOption.isDefined && asm.head == OP_0 && asm.contains(OP_CHECKMULTISIG)) {
      //must be p2sh because of bug that forces p2sh scripts
      //to begin with OP_0
      //scripSig for p2sh input script
      //OP_0 <scriptSig> <scriptSig> ... <scriptSig> <redeemScript>

      val (scriptSigs,_) = splitAtRedeemScript(asm)
      logger.info("Script sigs: " + scriptSigs)
      //filter out all of the PUSHDATA / BytesToPushOntoStack operations

      val scriptSigsWithoutPushOps = filterPushOps(scriptSigs)
      //remove the OP_0 that precedes every p2sh input script
      val scriptSigsWithoutPushOpsAndOp0 = scriptSigsWithoutPushOps.tail
      scriptSigsWithoutPushOpsAndOp0.map(sig => ECFactory.digitalSignature(sig.bytes))
    } else if (asm.headOption.isDefined && asm.head == OP_0) {
      //this means we have a traditional multisignature scriptSig
      val scriptSigs = asm.slice(1,asm.size)
      val scriptSigsWithoutPushOps = filterPushOps(scriptSigs)
      //remove the OP_0 that precedes every multisignature input script
      val scriptSigsWithoutPushOpsAndOp0 = scriptSigsWithoutPushOps.tail
      scriptSigsWithoutPushOpsAndOp0.map(sig => ECFactory.digitalSignature(sig.bytes))
      scriptSigsWithoutPushOps.map(sig => ECFactory.digitalSignature(sig.bytes))
    } else Seq(ECFactory.digitalSignature(asm(1).bytes))
  }*/

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

  /**
   * Filters out push operations in our scriptSig
   * this removes OP_PUSHDATA1, OP_PUSHDATA2, OP_PUSHDATA4 and all ByteToPushOntoStack tokens
   * @param asm
   * @return
   */
  def filterPushOps(asm : Seq[ScriptToken]) : Seq[ScriptToken] = {
    asm.filterNot(op => op.isInstanceOf[BytesToPushOntoStack]
      || op == OP_PUSHDATA1
      || op == OP_PUSHDATA2
      || op == OP_PUSHDATA4)
  }


  /**
   * Splits the given asm into two parts
   * the first part is the digital signatures
   * the second part is the redeem script
   * @param asm
   * @return
   */
  def splitAtRedeemScript(asm : Seq[ScriptToken]) : (Seq[ScriptToken],Seq[ScriptToken]) = {
    //using the first instance of a ScriptNumberOperation (i.e. OP_2, OP_3 etc...) as the beginning
    //of the redeemScript

    val result : Option[(ScriptToken,Int)] = asm.headOption match {
      case Some(OP_0) =>
        //skip the first index since OP_0 is put in input scripts because of a bug
        //in the original bitcoin implementation
        val r = asm.tail.zipWithIndex.find { case (token, index) => (token.isInstanceOf[ScriptNumberOperation])}
        //need to increment the result by one since the index is relative to the
        //tail of the list
        r.map(res => (res._1,res._2+1))
      case Some(_) => asm.zipWithIndex.find { case (token, index) => (token.isInstanceOf[ScriptNumberOperation]) }
      case None => asm.zipWithIndex.find { case (token, index) => (token.isInstanceOf[ScriptNumberOperation]) }
    }

    if (result.isDefined) asm.splitAt(result.get._2)
    else (asm,List())
  }
}

case class ScriptSignatureImpl(hex : String) extends ScriptSignature {

  def signatures : Seq[ECDigitalSignature]  = {
    if (asm.headOption.isDefined && asm.head == OP_0 && asm.contains(OP_CHECKMULTISIG)) {
      //must be p2sh because of bug that forces p2sh scripts
      //to begin with OP_0
      //scripSig for p2sh input script
      //OP_0 <scriptSig> <scriptSig> ... <scriptSig> <redeemScript>

      val (scriptSigs,_) = splitAtRedeemScript(asm)
      logger.info("Script sigs: " + scriptSigs)
      //filter out all of the PUSHDATA / BytesToPushOntoStack operations

      val scriptSigsWithoutPushOps = filterPushOps(scriptSigs)
      //remove the OP_0 that precedes every p2sh input script
      val scriptSigsWithoutPushOpsAndOp0 = scriptSigsWithoutPushOps.tail
      scriptSigsWithoutPushOpsAndOp0.map(sig => ECFactory.digitalSignature(sig.bytes))
    } else if (asm.headOption.isDefined && asm.head == OP_0) {
      //this means we have a traditional multisignature scriptSig
      val scriptSigs = asm.slice(1,asm.size)
      val scriptSigsWithoutPushOps = filterPushOps(scriptSigs)
      //remove the OP_0 that precedes every multisignature input script
      val scriptSigsWithoutPushOpsAndOp0 = scriptSigsWithoutPushOps.tail
      scriptSigsWithoutPushOpsAndOp0.map(sig => ECFactory.digitalSignature(sig.bytes))
      scriptSigsWithoutPushOps.map(sig => ECFactory.digitalSignature(sig.bytes))
    } else Seq(ECFactory.digitalSignature(asm(1).bytes))
  }
}


/**
 * P2PKH script signatures have only one public key
 * https://bitcoin.org/en/developer-guide#pay-to-public-key-hash-p2pkh
 * P2PKH scriptSigs follow this format
 * <sig> <pubkey>
 */
trait P2PKHScriptSignature extends ScriptSignature {

  /**
   * Gives us the public key inside of a p2pkh script signature
   * @return
   */
  def publicKeys : Seq[ECPublicKey] = Seq(ECFactory.publicKey(asm.last.bytes))


  def signatures : Seq[ECDigitalSignature] = {
    Seq(ECFactory.digitalSignature(asm(1).hex))
  }
}

/**
 * Represents a pay-to-script-hash script signature
 * https://bitcoin.org/en/developer-guide#pay-to-script-hash-p2sh
 * P2SH scriptSigs have the following format
 * <sig> [sig] [sig...] <redeemScript>
 */
trait P2SHScriptSignature extends ScriptSignature {
  /**
   * The redeemScript represents the conditions that must be satisfied to spend the output
   * @return
   */
  def redeemScript : Seq[ScriptToken] = splitAtRedeemScript(asm)._2

  /**
   * Returns the public keys for the p2sh scriptSignature
   * @return
   */
  def publicKeys : Seq[ECPublicKey] = {
    val pubKeys : Seq[ScriptToken] = redeemScript.filter(_.isInstanceOf[ScriptConstant])
    pubKeys.map(k => ECFactory.publicKey(k.hex))
  }

  def signatures : Seq[ECDigitalSignature] = {
    val nonRedeemScript = splitAtRedeemScript(asm)._1
    val sigs = nonRedeemScript.filter(_.isInstanceOf[ScriptConstant]).filterNot(_.isInstanceOf[ScriptNumberOperation])
    sigs.map(s => ECFactory.digitalSignature(s.hex))
  }

}

/**
 * Represents a multisignature script signature
 * https://bitcoin.org/en/developer-guide#multisig
 * Multisig script sigs have the following format
 * OP_0 <A sig> [B sig] [C sig...]
 */
trait MultiSignatureScriptSignature extends ScriptSignature {
  def signatures : Seq[ECDigitalSignature] = {
    asm.filter(_.isInstanceOf[ScriptConstant])
      .filterNot(_.isInstanceOf[ScriptNumberOperation])
      .map( sig => ECFactory.digitalSignature(sig.hex))
  }
}

/**
 * Represents a pay to public key script signature
 * https://bitcoin.org/en/developer-guide#pubkey
 */
trait PubKeyScriptSignature extends ScriptSignature {
  def signatures : Seq[ECDigitalSignature] = {
    Seq(ECFactory.digitalSignature(asm.head.hex))
  }
}


case class P2PKHScriptSignatureImpl(hex : String) extends P2PKHScriptSignature
case class P2SHScriptSignatureImpl(hex : String) extends P2SHScriptSignature
case class MultiSignatureScriptSignatureImpl(hex : String) extends MultiSignatureScriptSignature
case class PubKeyScriptSignatureImpl(hex : String) extends PubKeyScriptSignature