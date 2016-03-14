package org.scalacoin.crypto

import org.scalacoin.config.TestNet3
import org.scalacoin.protocol.script._
import org.scalacoin.protocol.transaction.{Transaction, TransactionInput}
import org.scalacoin.script.crypto.HashType
import org.scalacoin.util.{BitcoinSLogger, BitcoinSUtil}
import org.slf4j.LoggerFactory

/**
 * Created by chris on 2/16/16.
 * Responsible for checkign digital signatures on inputs against their respective
 * public keys
 */
trait TransactionSignatureChecker extends BitcoinSLogger {

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
      case p2pkScriptSignature : P2PKScriptSignature =>
        throw new RuntimeException("This is an old script signature type that is not supported by wallets anymore")
      case scriptPubKey : ScriptSignature =>
        throw new RuntimeException("We don't know how to check scriptSignatures of generic scriptPubKeys\n" +
          "scriptPubKey: " + scriptPubKey)
    }
  }


  /**
   * Checks a pay-to-pubkey-hash scriptSignature against the given scriptPubKey, transaction, and inputIndex
   * @param spendingTransaction
   * @param inputIndex
   * @param scriptPubKey
   * @param p2pkhScriptSig
   * @return
   */
  private def checkP2PKHScriptSignature(spendingTransaction : Transaction, inputIndex : Int, scriptPubKey : ScriptPubKey,
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

    /**
     * This is a helper function to check digital signatures against public keys
     * if the signature does not match this public key, check it against the next
     * public key in the sequence
     * @param sigs
     * @param pubKeys
     * @return
     */
    def helper(sigs : List[ECDigitalSignature], pubKeys : List[ECPublicKey]) : Boolean = {
      if (!sigs.isEmpty && !pubKeys.isEmpty) {
        val sig = sigs.head
        val pubKey = pubKeys.head
        val hashType = p2shScriptSignature.hashType(sig)
        val hashForSig = TransactionSignatureSerializer.hashForSignature(spendingTransaction,
          inputIndex,p2shScriptSignature.redeemScript,hashType)
        val result = pubKey.verify(hashForSig, sig)
        result match {
          case true => helper(sigs.tail,pubKeys.tail)
          case false => helper(sigs,pubKeys.tail)
        }
      } else if (pubKeys.isEmpty && !sigs.isEmpty) {
        //means that we have no more pubKeys to check signatures against, therefore
        //the validation for the tx fails
        false
      } else if (sigs.isEmpty) {
        //means that we have checked all of the sigs against the public keys
        //validation succeeds
        true
      } else false
    }
    scriptPubKey match {
      case x : P2SHScriptPubKey => helper(p2shScriptSignature.signatures.toList, p2shScriptSignature.publicKeys.toList)
      case x : MultiSignatureScriptPubKey =>
        logger.warn("Trying to check if a p2sScriptSignature spends a multisignature scriptPubKey properly - this is trivially false")
        false
      case x : P2PKHScriptPubKey =>
        logger.warn("Trying to check if a p2sScriptSignature spends a p2pkh scriptPubKey properly - this is trivially false")
        false
      case x : P2PKScriptPubKey =>
        logger.warn("Trying to check if a p2sScriptSignature spends a p2pk scriptPubKey properly - this is trivially false")
        false
      case x : NonStandardScriptPubKey =>
        logger.warn("Trying to check if a p2sScriptSignature spends a nonstandard scriptPubKey properly - this is trivially false")
        false
      case x : ScriptPubKey =>
        logger.warn("Trying to check if a p2sScriptSignature spends a scriptPubKey properly - this is trivially false")
        false
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
      case x: MultiSignatureScriptPubKey =>
        val result: Seq[Boolean] = for {
          (sig, pubKey) <- multiSignatureScript.signatures.zip(x.publicKeys)
        } yield {
            val hashType = multiSignatureScript.hashType(sig)
            val hashForSig: Seq[Byte] =
              TransactionSignatureSerializer.hashForSignature(spendingTransaction, inputIndex, x, hashType)
            pubKey.verify(hashForSig, sig)
          }
        !result.contains(false)
      case x: P2PKHScriptPubKey =>
        logger.warn("Trying to check if a multisignature scriptSig spends a p2pkh scriptPubKey properly - this is trivially false")
        false
      case x: P2PKScriptPubKey =>
        logger.warn("Trying to check if a multisignature scriptSig spends a p2pk scriptPubKey properly - this is trivially false")
        false
      case x: NonStandardScriptPubKey =>
        logger.warn("Trying to check if a multisignature scriptSig spends a p2sh scriptPubKey properly - this is trivially false")
        false
      case x: P2SHScriptPubKey =>
        logger.warn("Trying to check if a multisignature scriptSig spends a nonstandard scriptPubKey properly - this is trivially false")
        false
      case x: ScriptPubKey =>
        logger.warn("Trying to check if a multisignature scriptSig spends a scriptPubKey properly - this is trivially false")
        false
    }
  }

}

object TransactionSignatureChecker extends TransactionSignatureChecker
