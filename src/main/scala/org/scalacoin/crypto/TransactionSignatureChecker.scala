package org.scalacoin.crypto

import org.scalacoin.config.TestNet3
import org.scalacoin.protocol.script._
import org.scalacoin.protocol.transaction.{Transaction, TransactionInput}
import org.scalacoin.script.ScriptProgram
import org.scalacoin.script.crypto._
import org.scalacoin.script.flag.{ScriptFlagUtil, ScriptFlag, ScriptVerifyDerSig}
import org.scalacoin.util.{BitcoinSLogger, BitcoinSUtil}
import org.slf4j.LoggerFactory

import scala.annotation.tailrec

/**
 * Created by chris on 2/16/16.
 * Responsible for checking digital signatures on inputs against their respective
 * public keys
 */
trait TransactionSignatureChecker extends BitcoinSLogger {

  /**
   * Checks the signature of a scriptSig in the spending transaction against the
   * given scriptPubKey & explicitly given public key
   * This is useful for instances of non standard scriptSigs
   * @param spendingTransaction
   * @param inputIndex
   * @param scriptPubKey
   * @param pubKey
   * @return
   */
  def checkSignature(spendingTransaction : Transaction, inputIndex : Int, scriptPubKey : ScriptPubKey,
                     pubKey: ECPublicKey, signature : ECDigitalSignature, requireStrictDEREncoding : Boolean) : TransactionSignatureCheckerResult = {
    if (requireStrictDEREncoding && !DERSignatureUtil.isStrictDEREncoding(signature)) {
      logger.warn("Signature was not stricly encoded der: " + signature.hex)
      SignatureValidationFailureNotStrictDerEncoding
    } else {
      val hashTypeByte = if (signature.bytes.size > 0) signature.bytes.last else 0x00.toByte
      val hashType = HashTypeFactory.fromByte(hashTypeByte)
      val hashForSignature = TransactionSignatureSerializer.hashForSignature(spendingTransaction,inputIndex,scriptPubKey,hashType)
      logger.info("Hash for signature: " + BitcoinSUtil.encodeHex(hashForSignature))
      val isValid = pubKey.verify(hashForSignature,signature)
      if (isValid) SignatureValidationSuccess else SignatureValidationFailureIncorrectSignatures
    }

  }

  /**
   * This is a helper function to check digital signatures against public keys
   * if the signature does not match this public key, check it against the next
   * public key in the sequence
   * @param spendingTransaction the transaction being checked
   * @param inputIndex the input of the transaction being checked
   * @param scriptPubKey the scriptPubKey which the transaction's input is being checked against
   * @param sigs the signatures that are being checked for validity
   * @param pubKeys the public keys which are needed to verify that the signatures are correct
   * @param flags the script verify flags which are rules to verify the signatures
   * @return a boolean indicating if all of the signatures are valid against the given public keys
   */
  @tailrec
  final def multiSignatureEvaluator(txSignatureComponent : TransactionSignatureComponent,
                     sigs : List[ECDigitalSignature], pubKeys : List[ECPublicKey], flags : Seq[ScriptFlag],
                     requiredSigs : Long) : TransactionSignatureCheckerResult = {
    logger.info("Signatures inside of helper: " + sigs)
    logger.info("public keys inside of helper: " + pubKeys)
    if (sigs.size > pubKeys.size) {
      //this is how bitcoin core treats this. If there are ever any more
      //signatures than public keys remaining we immediately return
      //false https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.cpp#L955-L959
      logger.info("We have more sigs than we have public keys remaining")
      SignatureValidationFailureIncorrectSignatures
    }
    else if (requiredSigs > sigs.size) {
      logger.info("We do not have enough sigs to meet the threshold of requireSigs in the multiSignatureScriptPubKey")
      SignatureValidationFailureIncorrectSignatures
    }
    else if (!sigs.isEmpty && !pubKeys.isEmpty) {
      val sig = sigs.head
      if (ScriptFlagUtil.requiresStrictDerEncoding(flags) && !DERSignatureUtil.isStrictDEREncoding(sig)) {
        logger.warn("Signature for multi signature script was not strictly encoded: " + sig.hex)
        SignatureValidationFailureNotStrictDerEncoding
      } else {
        val pubKey = pubKeys.head
        val hashType = txSignatureComponent.scriptSignature.hashType(sig)
        val hashForSig = TransactionSignatureSerializer.hashForSignature(txSignatureComponent.transaction,
          txSignatureComponent.inputIndex,txSignatureComponent.scriptPubKey,hashType)
        val result = pubKey.verify(hashForSig, sig)
        result match {
          case true =>
            multiSignatureEvaluator(txSignatureComponent, sigs.tail,pubKeys.tail,flags, requiredSigs -1)
          case false =>
            multiSignatureEvaluator(txSignatureComponent, sigs,pubKeys.tail,flags, requiredSigs)
        }
      }
    } else if (sigs.isEmpty) {
      //means that we have checked all of the sigs against the public keys
      //validation succeeds
      SignatureValidationSuccess
    } else SignatureValidationFailureIncorrectSignatures
  }

}

object TransactionSignatureChecker extends TransactionSignatureChecker


