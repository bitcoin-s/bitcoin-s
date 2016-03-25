package org.scalacoin.protocol.script

import org.scalacoin.crypto.{ECFactory, ECPublicKey}
import org.scalacoin.marshallers.script.{ScriptParser, RawScriptPubKeyParser}
import org.scalacoin.marshallers.transaction.TransactionElement
import org.scalacoin.protocol._
import org.scalacoin.script.bitwise.{OP_EQUAL, OP_EQUALVERIFY}
import org.scalacoin.script.constant._
import org.scalacoin.script.crypto.{OP_CHECKMULTISIGVERIFY, OP_CHECKMULTISIG, OP_CHECKSIG, OP_HASH160}
import org.scalacoin.script.stack.OP_DUP
import org.scalacoin.util.BitcoinSLogger

/**
 * Created by chris on 12/26/15.
 */
sealed trait ScriptPubKey extends TransactionElement with BitcoinSLogger {

  /**
   * Representation of a scriptSignature in a parsed assembly format
   * this data structure can be run through the script interpreter to
   * see if a script evaluates to true
   * @return
   */
  def asm : Seq[ScriptToken]

  /**
   * Returns the script type of this scriptPubKey
   * @return
   */
  def scriptType : ScriptType = {
    asm match {
      case List(OP_DUP, OP_HASH160, BytesToPushOntoStackImpl(x), ScriptConstantImpl(pubKeyHash), OP_EQUALVERIFY, OP_CHECKSIG) => P2PKH
      case List(OP_HASH160, BytesToPushOntoStackImpl(x), ScriptConstantImpl(scriptHash), OP_EQUAL) => P2SH
      //TODO: make this more robust, this isn't the pattern that multsignature scriptPubKeys follow
      case _ if (asm.last == OP_CHECKMULTISIG) => MultiSignature
      case _ => NonStandard
    }
  }

  //the addresses that the bitcoins correlated to the output
  def addresses : Seq[BitcoinAddress] = ???

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
   * @return
   */
  def requiredSigs = {
    val asmWithoutPushOps = asm.filterNot(_.isInstanceOf[BytesToPushOntoStack])
    val opCheckMultiSigIndex = if (asm.indexOf(OP_CHECKMULTISIG) != -1) asmWithoutPushOps.indexOf(OP_CHECKMULTISIG) else asmWithoutPushOps.indexOf(OP_CHECKMULTISIGVERIFY)
    logger.debug("OP_CHECKMULTISIG index: " + opCheckMultiSigIndex)
    logger.debug("Max sigs: " + maxSigs)
    logger.debug("asm filter push ops: " + asmWithoutPushOps)
    //magic number 2 represents the maxSig operation and the OP_CHECKMULTISIG operation at the end of the asm
    val numSigsRequired = asmWithoutPushOps(opCheckMultiSigIndex - maxSigs.toInt - 2)
    logger.debug("num sigs required: " + numSigsRequired)
    numSigsRequired match {
      case x : ScriptNumber => x.num
      case _ => throw new RuntimeException("The first element of the multisignature pubkey must be a script number operation\n" +
      "scriptPubKey: " + this)
    }
  }

  /**
   * The maximum amount of signatures for this multisignature script pubkey output
   * @return
   */
  def maxSigs = {
    val checkMultiSigIndex = asm.indexOf(OP_CHECKMULTISIG)
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
   * Returns the public keys encoded into the scriptPubKey
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
trait P2PKScriptPubKey extends ScriptPubKey

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