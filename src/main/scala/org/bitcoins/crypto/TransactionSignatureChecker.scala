package org.bitcoins.crypto

import org.bitcoins.config.TestNet3
import org.bitcoins.protocol.script._
import org.bitcoins.protocol.transaction.{Transaction, TransactionInput}
import org.bitcoins.script.{ScriptProgram}
import org.bitcoins.script.crypto._
import org.bitcoins.script.flag.{ScriptFlagUtil, ScriptFlag, ScriptVerifyDerSig}
import org.bitcoins.util.{BitcoinScriptUtil, BitcoinSLogger, BitcoinSUtil}
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
 *
   * @param txSignatureComponent the tx signature component that contains all relevant tx information
   * @param pubKey
   * @return
   */
  def checkSignature(txSignatureComponent : TransactionSignatureComponent,
                     pubKey: ECPublicKey, signature : ECDigitalSignature, flags : Seq[ScriptFlag]) : TransactionSignatureCheckerResult = {
    val pubKeyEncodedCorrectly = BitcoinScriptUtil.checkPubKeyEncoding(pubKey,flags)
    if (ScriptFlagUtil.requiresStrictDerEncoding(flags) && !DERSignatureUtil.isStrictDEREncoding(signature)) {
      logger.error("Signature was not stricly encoded der: " + signature.hex)
      SignatureValidationFailureNotStrictDerEncoding
    } else if (!pubKeyEncodedCorrectly) {
      logger.error("The public key given for signature checking was not encoded correctly")
      SignatureValidationFailurePubKeyEncoding
    } else {
      //we need to check if the scriptSignature has a redeemScript
      //in that case, we need to pass the redeemScript to the TransactionSignatureChecker
      //we do this by setting the scriptPubKey inside of txSignatureComponent to the redeemScript
      //instead of the p2sh scriptPubKey it was previously
      //as the scriptPubKey instead of the one inside of ScriptProgram
      val txSignatureComponentWithScriptPubKeyAdjusted = txSignatureComponent.scriptSignature match {
        case s : P2SHScriptSignature => TransactionSignatureComponentFactory.factory(txSignatureComponent,s.redeemScript)
        case _ : P2PKHScriptSignature | _ : P2PKScriptSignature | _ : NonStandardScriptSignature
             | _ : MultiSignatureScriptSignature | EmptyScriptSignature => txSignatureComponent
      }
      val hashTypeByte = if (signature.bytes.size > 0) signature.bytes.last else 0x00.toByte
      val hashType = HashTypeFactory.fromByte(hashTypeByte)
      val hashForSignature = TransactionSignatureSerializer.hashForSignature(txSignatureComponentWithScriptPubKeyAdjusted.transaction,
        txSignatureComponentWithScriptPubKeyAdjusted.inputIndex,txSignatureComponentWithScriptPubKeyAdjusted.scriptPubKey,hashType)
      logger.info("Hash for signature: " + BitcoinSUtil.encodeHex(hashForSignature))
      val isValid = pubKey.verify(hashForSignature,signature)
      if (isValid) SignatureValidationSuccess else SignatureValidationFailureIncorrectSignatures
    }
  }

  /**
   * This is a helper function to check digital signatures against public keys
   * if the signature does not match this public key, check it against the next
   * public key in the sequence
 *
   * @param txSignatureComponent the tx signature component that contains all relevant transaction information
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
    logger.info("Public keys inside of helper: " + pubKeys)
    if (sigs.size > pubKeys.size) {
      //this is how bitcoin core treats this. If there are ever any more
      //signatures than public keys remaining we immediately return
      //false https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.cpp#L955-L959
      logger.info("We have more sigs than we have public keys remaining")
      SignatureValidationFailureIncorrectSignatures
    }
    else if (requiredSigs > sigs.size) {
      //for the case when we do not have enough sigs left to check to meet the required signature threshold
      //https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.cpp#L914-915
      logger.info("We do not have enough sigs to meet the threshold of requireSigs in the multiSignatureScriptPubKey")
      SignatureValidationFailureSignatureCount
    }
    else if (!sigs.isEmpty && !pubKeys.isEmpty) {
      val sig = sigs.head
      val pubKey = pubKeys.head
      val result = checkSignature(txSignatureComponent,pubKey,sig,flags)
      result match {
        case SignatureValidationSuccess =>
          multiSignatureEvaluator(txSignatureComponent, sigs.tail,pubKeys.tail,flags, requiredSigs -1)
        case SignatureValidationFailureIncorrectSignatures =>
          multiSignatureEvaluator(txSignatureComponent, sigs,pubKeys.tail,flags, requiredSigs)
        case SignatureValidationFailureNotStrictDerEncoding =>
          SignatureValidationFailureNotStrictDerEncoding
        case SignatureValidationFailureSignatureCount =>
          SignatureValidationFailureSignatureCount
        case SignatureValidationFailurePubKeyEncoding =>
          SignatureValidationFailurePubKeyEncoding
      }
    } else if (sigs.isEmpty) {
      //means that we have checked all of the sigs against the public keys
      //validation succeeds
      SignatureValidationSuccess
    } else SignatureValidationFailureIncorrectSignatures
  }

}

object TransactionSignatureChecker extends TransactionSignatureChecker


