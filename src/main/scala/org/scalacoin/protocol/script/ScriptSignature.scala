package org.scalacoin.protocol.script

import org.scalacoin.crypto.{ECPublicKey, ECFactory, ECDigitalSignature}
import org.scalacoin.marshallers.script.{RawScriptSignatureParser, RawScriptPubKeyParser, ScriptParser}
import org.scalacoin.marshallers.transaction.TransactionElement

import org.scalacoin.script.constant._
import org.scalacoin.script.crypto.{OP_CHECKMULTISIG, HashType, HashTypeFactory}
import org.scalacoin.util.{BitcoinSLogger, BitcoinSUtil}
import org.slf4j.LoggerFactory

/**
 * Created by chris on 12/26/15.
 *
 */
sealed trait ScriptSignature extends TransactionElement with ScriptSignatureFactory with BitcoinSLogger {

  /**
   * Representation of a scriptSignature in a parsed assembly format
   * this data structure can be run through the script interpreter to
   * see if a script evaluates to true
   * @return
   */
  def asm : Seq[ScriptToken]

  /**
   * The digital signatures contained inside of the script signature
   * p2pkh script signatures only have one sig
   * p2pk script signatures only have one sigs
   * p2sh script signatures can have m sigs
   * multisignature scripts can have m sigs
   * @return
   */
  def signatures : Seq[ECDigitalSignature]

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


}

trait NonStandardScriptSignature extends ScriptSignature {
  def signatures : Seq[ECDigitalSignature]  = ???
}

object NonStandardScriptSignatureImpl {
  def apply(hex : String) : NonStandardScriptSignatureImpl = NonStandardScriptSignatureImpl(hex, RawScriptSignatureParser.read(hex).asm)
}
case class NonStandardScriptSignatureImpl(hex : String, asm : Seq[ScriptToken]) extends NonStandardScriptSignature


/**
 * P2PKH script signatures have only one public key
 * https://bitcoin.org/en/developer-guide#pay-to-public-key-hash-p2pkh
 * P2PKH scriptSigs follow this format
 * <sig> <pubkey>
 */
trait P2PKHScriptSignature extends ScriptSignature {


  /**
   * P2PKH scriptSigs only have one signature
   * @return
   */
  def signature : ECDigitalSignature = signatures.head

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
  def redeemScript : ScriptPubKey = ScriptPubKeyFactory.fromBytes(asm.last.bytes)

  /**
   * Returns the public keys for the p2sh scriptSignature
   * @return
   */
  def publicKeys : Seq[ECPublicKey] = {
    val pubKeys : Seq[ScriptToken] = redeemScript.asm.filter(_.isInstanceOf[ScriptConstant])
      .filterNot(_.isInstanceOf[ScriptNumberOperation])
    pubKeys.map(k => ECFactory.publicKey(k.hex))
  }

  /**
   * The digital signatures inside of the scriptSig
   * @return
   */
  def signatures : Seq[ECDigitalSignature] = {
    val nonRedeemScript = splitAtRedeemScript(asm)._1
    val sigs = nonRedeemScript.filter(_.isInstanceOf[ScriptConstant]).filterNot(_.isInstanceOf[ScriptNumberOperation])
    sigs.map(s => ECFactory.digitalSignature(s.hex))
  }


  /**
   * Splits the given asm into two parts
   * the first part is the digital signatures
   * the second part is the redeem script
   * @param asm
   * @return
   */
  def splitAtRedeemScript(asm : Seq[ScriptToken]) : (Seq[ScriptToken],Seq[ScriptToken]) = {
    (asm.reverse.tail.reverse, Seq(asm.last))
  }

}

/**
 * Represents a multisignature script signature
 * https://bitcoin.org/en/developer-guide#multisig
 * Multisig script sigs have the following format
 * OP_0 <A sig> [B sig] [C sig...]
 */
trait MultiSignatureScriptSignature extends ScriptSignature {
  /**
   * The digital signatures inside of the scriptSig
   * @return
   */
  def signatures : Seq[ECDigitalSignature] = {
    asm.filter(_.isInstanceOf[ScriptConstant])
      .filterNot(_.isInstanceOf[ScriptNumberOperation])
      .map(sig => ECFactory.digitalSignature(sig.hex))
  }
}

/**
 * Represents a pay to public key script signature
 * https://bitcoin.org/en/developer-guide#pubkey
 * Signature script: <sig>
 */
trait P2PKScriptSignature extends ScriptSignature {


  /**
   * PubKey scriptSignatures only have one signature
   * @return
   */
  def signature : ECDigitalSignature = signatures.head
  /**
   * The digital signatures inside of the scriptSig
   * @return
   */
  def signatures : Seq[ECDigitalSignature] = {
    Seq(ECFactory.digitalSignature(asm.head.hex))
  }
}

object P2PKHScriptSignatureImpl {
  def apply(hex : String) : P2PKHScriptSignatureImpl = P2PKHScriptSignatureImpl(hex, RawScriptSignatureParser.read(hex).asm)
}
case class P2PKHScriptSignatureImpl(hex : String, asm : Seq[ScriptToken]) extends P2PKHScriptSignature

object P2SHScriptSignatureImpl {
  def apply(hex : String) : P2SHScriptSignatureImpl = P2SHScriptSignatureImpl(hex, RawScriptSignatureParser.read(hex).asm)
}
case class P2SHScriptSignatureImpl(hex : String, asm : Seq[ScriptToken]) extends P2SHScriptSignature

object MultiSignatureScriptSignatureImpl {
  def apply(hex : String) : MultiSignatureScriptSignatureImpl = MultiSignatureScriptSignatureImpl(hex, RawScriptSignatureParser.read(hex).asm)
}
case class MultiSignatureScriptSignatureImpl(hex : String, asm : Seq[ScriptToken]) extends MultiSignatureScriptSignature

object P2PKScriptSignatureImpl {
  def apply(hex : String) : P2PKScriptSignatureImpl = P2PKScriptSignatureImpl(hex, RawScriptSignatureParser.read(hex).asm)
}
case class P2PKScriptSignatureImpl(hex : String, asm : Seq[ScriptToken]) extends P2PKScriptSignature

/**
 * Represents the empty script signature
 */
case object EmptyScriptSignature extends ScriptSignature {
  def asm = List()
  def signatures = List()
  def hex = ""
}