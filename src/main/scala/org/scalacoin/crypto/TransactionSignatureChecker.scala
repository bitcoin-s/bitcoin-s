package org.scalacoin.crypto

import org.scalacoin.protocol.script.ScriptPubKey
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

}

object TransactionSignatureChecker extends TransactionSignatureChecker
