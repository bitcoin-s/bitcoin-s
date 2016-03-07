package org.scalacoin.crypto

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
        val hashType = p2pkhScriptSig.hashType(p2pkhScriptSig.signatures.head)
        val hashForSignature : Seq[Byte] =
          TransactionSignatureSerializer.hashForSignature(spendingTransaction,inputIndex,scriptPubKey,hashType)
        p2pkhScriptSig.publicKeys.head.verify(hashForSignature,p2pkhScriptSig.signatures.head)

      case multiSignatureScript : MultiSignatureScriptSignature =>
        scriptPubKey match {
          case x : MultiSignatureScriptPubKey =>
            val result : Seq[Boolean] = for {
              (sig,pubKey) <- multiSignatureScript.signatures.zip(x.publicKeys)
            } yield {
                val hashType = multiSignatureScript.hashType(sig)
                val hashForSig : Seq[Byte] =
                  TransactionSignatureSerializer.hashForSignature(spendingTransaction,inputIndex,scriptPubKey,hashType)
                pubKey.verify(hashForSig,sig)
              }
            !result.exists(_ == false)
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


  /**
   * Checks the list of signatures against the list of public keys
   * sigs are checked against the public key at the corresponding index
   * @param sigs
   * @param pubKeys
   * @return
   */
  private def checkSigsAgainstPubKeys(spendingTx : Transaction, inputIndex : Int, sigs : Seq[ECDigitalSignature], pubKeys : Seq[ECPublicKey]) : Boolean = {
    require(sigs.size == pubKeys.size, "You gave a different amount of signatures to check than public keys provided")
/*    val result : Seq[Boolean] = for {
      (sig,pubKey) <- (sigs.zip(pubKeys))
    } yield {
        val hashForSig = TransactionSignatureSerializer.hashForSignature(spendingTx,inputIndex,)
        pubKey.verify(_,sig)
      }*/
    ???
  }

}

object TransactionSignatureChecker extends TransactionSignatureChecker
