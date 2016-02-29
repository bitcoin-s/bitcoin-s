package org.scalacoin.crypto

import org.scalacoin.protocol.script.ScriptPubKey
import org.scalacoin.protocol.transaction.{Transaction, TransactionInput}
import org.scalacoin.script.crypto.HashType

/**
 * Created by chris on 2/16/16.
 */
trait TransactionSignatureChecker {


  /**
   * Checks the signature of a scriptSig in the spending transaction against the
   * given scriptPubKey
   * @param spendingTransaction
   * @param inputIndex
   * @param scriptPubKey
   * @param signature
   * @param pubKey
   * @param hashType
   * @return
   */
  def checkSignature(spendingTransaction : Transaction, inputIndex : Int, scriptPubKey : ScriptPubKey,
                      signature : ECDigitalSignature, pubKey: ECPublicKey, hashType : HashType) : Boolean = {
    val hashForSignature = TransactionSignatureSerializer.hashForSignature(spendingTransaction,inputIndex,scriptPubKey,hashType)
    val isValid = pubKey.verify(hashForSignature,signature)
    isValid
  }

}

object TransactionSignatureChecker extends TransactionSignatureChecker
