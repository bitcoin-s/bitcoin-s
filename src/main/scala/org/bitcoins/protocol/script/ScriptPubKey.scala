package org.bitcoins.protocol.script

import org.bitcoins.crypto.{ECFactory, ECPublicKey}
import org.bitcoins.marshallers.script.{ScriptParser, RawScriptPubKeyParser}
import org.bitcoins.marshallers.transaction.TransactionElement
import org.bitcoins.protocol._
import org.bitcoins.script.bitwise.{OP_EQUAL, OP_EQUALVERIFY}
import org.bitcoins.script.constant._
import org.bitcoins.script.crypto.{OP_CHECKMULTISIGVERIFY, OP_CHECKMULTISIG, OP_CHECKSIG, OP_HASH160}
import org.bitcoins.script.stack.OP_DUP
import org.bitcoins.util.{Factory, BitcoinScriptUtil, BitcoinSLogger}

/**
 * Created by chris on 12/26/15.
 */
sealed trait ScriptPubKey extends TransactionElement with BitcoinSLogger {

  /**
   * Representation of a scriptSignature in a parsed assembly format
   * this data structure can be run through the script interpreter to
   * see if a script evaluates to true
 *
   * @return
   */
  def asm : Seq[ScriptToken]


}

/**
 * Represents a pay-to-pubkey hash script pubkey
 * https://bitcoin.org/en/developer-guide#pay-to-public-key-hash-p2pkh
 * Format: OP_DUP OP_HASH160 <PubKeyHash> OP_EQUALVERIFY OP_CHECKSIG
 */
trait P2PKHScriptPubKey extends ScriptPubKey

/**
 * Represents a multisignature script public key
 * https://bitcoin.org/en/developer-guide#multisig
 * Format: <m> <A pubkey> [B pubkey] [C pubkey...] <n> OP_CHECKMULTISIG
 */
trait MultiSignatureScriptPubKey extends ScriptPubKey {


  /**
   * Returns the amount of required signatures for this multisignature script pubkey output
 *
   * @return
   */
  def requiredSigs : Long = {
    val asmWithoutPushOps = asm.filterNot(_.isInstanceOf[BytesToPushOntoStack])
    val opCheckMultiSigIndex = if (asm.indexOf(OP_CHECKMULTISIG) != -1) asmWithoutPushOps.indexOf(OP_CHECKMULTISIG) else asmWithoutPushOps.indexOf(OP_CHECKMULTISIGVERIFY)
    logger.debug("opCheckMultiSigIndex: " + opCheckMultiSigIndex)
    logger.debug("maxSigs: " + maxSigs)
    logger.debug("asmWithoutPushOps: " + asmWithoutPushOps)
    //magic number 2 represents the maxSig operation and the OP_CHECKMULTISIG operation at the end of the asm
    val numSigsRequired = asmWithoutPushOps(opCheckMultiSigIndex - maxSigs.toInt - 2)
    numSigsRequired match {
      case x : ScriptNumber => x.num
      case _ => throw new RuntimeException("The first element of the multisignature pubkey must be a script number operation\n" +
        "operation: " + numSigsRequired +
        "\nscriptPubKey: " + this)
    }
  }

  /**
   * The maximum amount of signatures for this multisignature script pubkey output
 *
   * @return
   */
  def maxSigs : Long = {
    if (checkMultiSigIndex == -1 || checkMultiSigIndex == 0) {
      //means that we do not have a max signature requirement
      0.toLong
    } else {
      asm(checkMultiSigIndex - 1) match {
        case x : ScriptNumber => x.num
        case _ => throw new RuntimeException("The element preceding a OP_CHECKMULTISIG operation in a  multisignature pubkey must be a script number operation")
      }
    }
  }


  /**
   * Gives the OP_CHECKMULTISIG or OP_CHECKMULTISIGVERIFY index inside of asm
 *
   * @return the index of OP_CHECKMULTISIG or OP_CHECKMULTISIGVERIFY
   */
  private def checkMultiSigIndex : Int = {
    if (asm.indexOf(OP_CHECKMULTISIG) != -1) asm.indexOf(OP_CHECKMULTISIG) else asm.indexOf(OP_CHECKMULTISIGVERIFY)
  }

  /**
   * Returns the public keys encoded into the scriptPubKey
 *
   * @return
   */
  def publicKeys : Seq[ECPublicKey] = {
    asm.filter(_.isInstanceOf[ScriptConstant]).slice(1, maxSigs.toInt + 1).map(key => ECFactory.publicKey(key.hex))
  }
}

/**
 * Represents a pay-to-scripthash public key
 * https://bitcoin.org/en/developer-guide#pay-to-script-hash-p2sh
 * Format: OP_HASH160 <Hash160(redeemScript)> OP_EQUAL
 */
trait P2SHScriptPubKey extends ScriptPubKey

/**
 * Represents a pay to public key script public key
 * https://bitcoin.org/en/developer-guide#pubkey
 * Format: <pubkey> OP_CHECKSIG
 */
trait P2PKScriptPubKey extends ScriptPubKey {
  def publicKey = ECFactory.publicKey(BitcoinScriptUtil.filterPushOps(asm).head.bytes)
}

trait NonStandardScriptPubKey extends ScriptPubKey


object NonStandardScriptPubKeyImpl {
  def apply(hex : String) : NonStandardScriptPubKeyImpl = NonStandardScriptPubKeyImpl(hex, RawScriptPubKeyParser.read(hex).asm)
}
case class NonStandardScriptPubKeyImpl(hex : String, asm : Seq[ScriptToken]) extends NonStandardScriptPubKey

object P2PKHScriptPubKeyImpl {
  def apply(hex : String) : P2PKHScriptPubKeyImpl = P2PKHScriptPubKeyImpl(hex, RawScriptPubKeyParser.read(hex).asm)
}
case class P2PKHScriptPubKeyImpl(hex : String, asm : Seq[ScriptToken]) extends P2PKHScriptPubKey


object MultiSignatureScriptPubKeyImpl {
  def apply(hex : String) : MultiSignatureScriptPubKeyImpl = MultiSignatureScriptPubKeyImpl(hex, RawScriptPubKeyParser.read(hex).asm)
}
case class MultiSignatureScriptPubKeyImpl(hex : String,asm : Seq[ScriptToken]) extends MultiSignatureScriptPubKey

object P2SHScriptPubKeyImpl {
  def apply(hex : String) : P2SHScriptPubKeyImpl = P2SHScriptPubKeyImpl(hex, RawScriptPubKeyParser.read(hex).asm)
}
case class P2SHScriptPubKeyImpl(hex : String,asm : Seq[ScriptToken]) extends P2SHScriptPubKey

object P2PKScriptPubKeyImpl {
  def apply(hex : String) : P2PKScriptPubKeyImpl = P2PKScriptPubKeyImpl(hex, RawScriptPubKeyParser.read(hex).asm)
}
case class P2PKScriptPubKeyImpl(hex : String,asm : Seq[ScriptToken]) extends P2PKScriptPubKey

/**
 * Represents the empty script pub key
 */
case object EmptyScriptPubKey extends ScriptPubKey {
  def asm = List()
  def hex = ""
}

/**
  * Factory companion object used to create ScriptPubKey objects
  */
object ScriptPubKey extends Factory[ScriptPubKey] {
  def empty : ScriptPubKey = fromAsm(List())

  def fromBytes(bytes : Seq[Byte]) : ScriptPubKey = RawScriptPubKeyParser.read(bytes)

  /**
    * Creates a scriptPubKey from its asm representation
 *
    * @param asm
    * @return
    */
  def fromAsm(asm : Seq[ScriptToken]) : ScriptPubKey = {
    val scriptPubKeyHex = BitcoinScriptUtil.asmToHex(asm)
    asm match {
      case Seq() => EmptyScriptPubKey
      case List(OP_DUP, OP_HASH160, x : BytesToPushOntoStack, ScriptConstantImpl(pubKeyHash), OP_EQUALVERIFY, OP_CHECKSIG) =>
        P2PKHScriptPubKeyImpl(scriptPubKeyHex,asm)
      case List(OP_HASH160, x : BytesToPushOntoStack, ScriptConstantImpl(scriptHash), OP_EQUAL) =>
        P2SHScriptPubKeyImpl(scriptPubKeyHex,asm)
      case List(b : BytesToPushOntoStack, x : ScriptConstant, OP_CHECKSIG) => P2PKScriptPubKeyImpl(scriptPubKeyHex,asm)
      case _ if (isMultiSignatureScriptPubKey(asm)) =>
        MultiSignatureScriptPubKeyImpl(scriptPubKeyHex,asm)
      case _ => NonStandardScriptPubKeyImpl(scriptPubKeyHex,asm)
    }
  }


  /**
    * Determines if the given script tokens are a multisignature scriptPubKey
 *
    * @param asm the tokens to check
    * @return a boolean indicating if the given tokens are a multisignature scriptPubKey
    */
  private def isMultiSignatureScriptPubKey(asm : Seq[ScriptToken]) : Boolean = {
    val isNotEmpty = asm.size > 0
    val containsMultSigOp = asm.contains(OP_CHECKMULTISIG) || asm.contains(OP_CHECKMULTISIGVERIFY)
    //we need at least two script operations to indicate m required signatures & n maximum signatures
    val has2ScriptOperations = asm.count(_.isInstanceOf[ScriptNumberOperation]) >= 2
    isNotEmpty && containsMultSigOp && has2ScriptOperations

  }

  def apply(bytes: Seq[Byte]) : ScriptPubKey = fromBytes(bytes)

  def apply(hex : String) : ScriptPubKey = fromHex(hex)

}