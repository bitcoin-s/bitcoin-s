package org.scalacoin.crypto

import org.scalacoin.config.TestNet3
import org.scalacoin.protocol.script._
import org.scalacoin.protocol.transaction.{Transaction, TransactionInput}
import org.scalacoin.script.crypto.HashType
import org.scalacoin.util.BitcoinSUtil
import org.slf4j.LoggerFactory

/**
 * Created by chris on 2/16/16.
 */
trait TransactionSignatureChecker {


  private def logger = LoggerFactory.getLogger(this.getClass())
  /**
   * Checks the signature of a scriptSig in the spending transaction against the
   * given scriptPubKey
   * @param spendingTransaction
   * @param inputIndex
   * @param scriptPubKey
   * @param pubKey
   * @return
   */
  def checkSignature(spendingTransaction : Transaction, inputIndex : Int, scriptPubKey : ScriptPubKey,
                     pubKey: ECPublicKey) : Boolean = {
    val input = spendingTransaction.inputs(inputIndex)
    val signature = input.scriptSignature.signatures.head
    val hashType = input.scriptSignature.hashType(signature)
    val hashForSignature = TransactionSignatureSerializer.hashForSignature(spendingTransaction,inputIndex,scriptPubKey,hashType)
    logger.info("Hash for signature: " + BitcoinSUtil.encodeHex(hashForSignature))

    val isValid = pubKey.verify(hashForSignature,signature)
    isValid
  }

  /**
   * Checks the signatures on a given input against the scriptPubKey
   * @param spendingTransaction
   * @param inputIndex
   * @param scriptPubKey
   * @return
   */
  def checkSignature(spendingTransaction : Transaction, inputIndex : Int, scriptPubKey: ScriptPubKey) : Boolean = {
    val input = spendingTransaction.inputs(inputIndex)
    val scriptSig = input.scriptSignature
    scriptSig match {
      case p2pkhScriptSig : P2PKHScriptSignature =>
        checkP2PKHScriptSignature(spendingTransaction,inputIndex,scriptPubKey, p2pkhScriptSig)
      case multiSignatureScript : MultiSignatureScriptSignature =>
        checkMultiSignatureScriptSig(spendingTransaction,inputIndex,scriptPubKey,multiSignatureScript)
      case p2shSignatureScript : P2SHScriptSignature =>
        checkP2SHScriptSignature(spendingTransaction,inputIndex,scriptPubKey, p2shSignatureScript)

    }
  }


  def checkP2PKHScriptSignature(spendingTransaction : Transaction, inputIndex : Int, scriptPubKey : ScriptPubKey,
                                p2pkhScriptSig : P2PKHScriptSignature) : Boolean = {
    val hashType = p2pkhScriptSig.hashType(p2pkhScriptSig.signatures.head)
    val hashForSignature : Seq[Byte] =
      TransactionSignatureSerializer.hashForSignature(spendingTransaction,inputIndex,scriptPubKey,hashType)
    p2pkhScriptSig.publicKeys.head.verify(hashForSignature,p2pkhScriptSig.signatures.head)
  }

  /**
   * Checks the p2sh scriptsig against the given scriptPubKey
   * throws an exception if the given scriptPubKey isn't a P2SHScriptPubKey
   * @param spendingTransaction
   * @param inputIndex
   * @param scriptPubKey
   * @param p2shScriptSignature
   * @return
   */
  private def checkP2SHScriptSignature(spendingTransaction : Transaction, inputIndex : Int, scriptPubKey : ScriptPubKey,
                                       p2shScriptSignature : P2SHScriptSignature) : Boolean = {
    scriptPubKey match {
      case x : P2SHScriptPubKey =>
        val result : Seq[Boolean] = for {
          (sig,pubKey) <- p2shScriptSignature.signatures.zip(p2shScriptSignature.publicKeys)
        } yield {
          val hashType = p2shScriptSignature.hashType(sig)
          val hashForSig = TransactionSignatureSerializer.hashForSignature(spendingTransaction,
            inputIndex,p2shScriptSignature.redeemScript,hashType)
          pubKey.verify(hashForSig, sig)
        }
        !result.contains(false)
      case x : MultiSignatureScriptPubKey =>
        throw new RuntimeException("Cannot check p2sh script signature against a non p2sh scriptPubKey type")
      case x : P2PKHScriptPubKey =>
        throw new RuntimeException("Cannot check p2sh script signature against a non p2sh scriptPubKey type")
      case x : P2PKScriptPubKey =>
        throw new RuntimeException("Cannot check p2sh script signature against a non p2sh scriptPubKey type")
      case x : NonStandardScriptPubKey =>
        throw new RuntimeException("Cannot check p2sh script signature against a non p2sh scriptPubKey type")
    }
  }
  /**
   * Checks a multisignature script sig against the given scriptPubKey
   * throws and exception if the given scriptPubKey is not of type MultiSignatureScriptPubKey
   * @param spendingTransaction
   * @param inputIndex
   * @param scriptPubKey
   * @param multiSignatureScript
   * @return
   */
  private def checkMultiSignatureScriptSig(spendingTransaction : Transaction, inputIndex : Int, scriptPubKey : ScriptPubKey,
                                           multiSignatureScript : MultiSignatureScriptSignature) : Boolean = {
    scriptPubKey match {
      case x : MultiSignatureScriptPubKey =>
        val result : Seq[Boolean] = for {
          (sig,pubKey) <- multiSignatureScript.signatures.zip(x.publicKeys)
        } yield {
            val hashType = multiSignatureScript.hashType(sig)
            val hashForSig : Seq[Byte] =
              TransactionSignatureSerializer.hashForSignature(spendingTransaction,inputIndex,x,hashType)
            pubKey.verify(hashForSig,sig)
          }
        !result.contains(false)
      case x : P2PKHScriptPubKey =>
        throw new RuntimeException("Cannot check multisignature script signature against a non multisignature scriptPubKey type")
      case y : P2SHScriptPubKey =>
        throw new RuntimeException("Cannot check multisignature script signature against a non multisignature scriptPubKey type")
      case z : P2PKScriptPubKey =>
        throw new RuntimeException("Cannot check multisignature script signature against a non multisignature scriptPubKey type")
      case q : NonStandardScriptPubKey =>
        throw new RuntimeException("Cannot check multisignature script signature against a non multisignature scriptPubKey type")
    }
  }

}

object TransactionSignatureChecker extends TransactionSignatureChecker
