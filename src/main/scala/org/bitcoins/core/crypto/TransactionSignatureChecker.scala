package org.bitcoins.core.crypto

import org.bitcoins.core.script.constant.ScriptToken
import org.bitcoins.core.script.crypto._
import org.bitcoins.core.script.flag.{ ScriptFlag, ScriptFlagUtil }
import org.bitcoins.core.script.result.ScriptErrorWitnessPubKeyType
import org.bitcoins.core.util.{ BitcoinSLogger, BitcoinSUtil, BitcoinScriptUtil }

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
   * @param txSignatureComponent the relevant transaction information for signature checking
   * @param script the current script state inside the interpreter - this is needed in the case of OP_CODESEPARATORS
   * @param pubKey the public key the signature is being checked against
   * @param signature the signature which is being checked against the transaction & the public key
   * @param flags the script flags used to check validity of the signature
   * @return a boolean indicating if the signature is valid or not
   */
  def checkSignature(txSignatureComponent: TxSigComponent, script: Seq[ScriptToken],
                     pubKey: ECPublicKey, signature: ECDigitalSignature, flags: Seq[ScriptFlag]): TransactionSignatureCheckerResult = {
    logger.debug("Signature: " + signature)
    val pubKeyEncodedCorrectly = BitcoinScriptUtil.isValidPubKeyEncoding(pubKey, flags)
    if (ScriptFlagUtil.requiresStrictDerEncoding(flags) && !DERSignatureUtil.isValidSignatureEncoding(signature)) {
      logger.error("Signature was not stricly encoded der: " + signature.hex)
      SignatureValidationErrorNotStrictDerEncoding
    } else if (ScriptFlagUtil.requireLowSValue(flags) && !DERSignatureUtil.isLowS(signature)) {
      logger.error("Signature did not have a low s value")
      SignatureValidationErrorHighSValue
    } else if (ScriptFlagUtil.requireStrictEncoding(flags) && signature.bytes.nonEmpty &&
      !HashType.isDefinedHashtypeSignature(signature)) {
      logger.error("signature: " + signature.hex)
      logger.error("Hash type was not defined on the signature, got: " + signature.bytes.last)
      SignatureValidationErrorHashType
    } else if (pubKeyEncodedCorrectly.isDefined) {
      val err = pubKeyEncodedCorrectly.get
      val result = if (err == ScriptErrorWitnessPubKeyType) SignatureValidationErrorWitnessPubKeyType else SignatureValidationErrorPubKeyEncoding
      logger.error("The public key given for signature checking was not encoded correctly, err: " + result)
      result
    } else {
      val sigsRemovedScript = BitcoinScriptUtil.calculateScriptForChecking(txSignatureComponent, signature, script)
      val hashTypeByte = if (signature.bytes.nonEmpty) signature.bytes.last else 0x00.toByte
      val hashType = HashType(Seq(0.toByte, 0.toByte, 0.toByte, hashTypeByte))

      val hashForSignature = txSignatureComponent match {
        case b: BaseTxSigComponent =>
          TransactionSignatureSerializer.hashForSignature(
            txSignatureComponent.transaction,
            txSignatureComponent.inputIndex,
            sigsRemovedScript, hashType
          )
        case w: WitnessTxSigComponent =>
          TransactionSignatureSerializer.hashForSignature(w.transaction, w.inputIndex, sigsRemovedScript, hashType,
            w.amount, w.sigVersion)
        case r: WitnessTxSigComponentRebuilt =>
          TransactionSignatureSerializer.hashForSignature(r.transaction, r.inputIndex, sigsRemovedScript, hashType,
            r.amount, r.sigVersion)
      }

      logger.debug("Hash for signature: " + BitcoinSUtil.encodeHex(hashForSignature.bytes))
      val sigWithoutHashType = stripHashType(signature)
      val isValid = pubKey.verify(hashForSignature, sigWithoutHashType)
      if (isValid) SignatureValidationSuccess
      else nullFailCheck(Seq(signature), SignatureValidationErrorIncorrectSignatures, flags)
    }
  }

  /**
   * This is a helper function to check digital signatures against public keys
   * if the signature does not match this public key, check it against the next
   * public key in the sequence
   * @param txSignatureComponent the tx signature component that contains all relevant transaction information
   * @param script the script state this is needed in case there is an OP_CODESEPARATOR inside the script
   * @param sigs the signatures that are being checked for validity
   * @param pubKeys the public keys which are needed to verify that the signatures are correct
   * @param flags the script verify flags which are rules to verify the signatures
   * @return a boolean indicating if all of the signatures are valid against the given public keys
   */
  @tailrec
  final def multiSignatureEvaluator(txSignatureComponent: TxSigComponent, script: Seq[ScriptToken],
                                    sigs: List[ECDigitalSignature], pubKeys: List[ECPublicKey], flags: Seq[ScriptFlag],
                                    requiredSigs: Long): TransactionSignatureCheckerResult = {
    logger.debug("Signatures inside of helper: " + sigs)
    logger.debug("Public keys inside of helper: " + pubKeys)
    if (sigs.size > pubKeys.size) {
      //this is how bitcoin core treats this. If there are ever any more
      //signatures than public keys remaining we immediately return
      //false https://github.com/bitcoin/bitcoin/blob/8c1dbc5e9ddbafb77e60e8c4e6eb275a3a76ac12/src/script/interpreter.cpp#L943-L945
      logger.warn("We have more sigs than we have public keys remaining")
      nullFailCheck(sigs, SignatureValidationErrorIncorrectSignatures, flags)
    } else if (requiredSigs > sigs.size) {
      //for the case when we do not have enough sigs left to check to meet the required signature threshold
      //https://github.com/bitcoin/bitcoin/blob/8c1dbc5e9ddbafb77e60e8c4e6eb275a3a76ac12/src/script/interpreter.cpp#L990-L991
      logger.warn("We do not have enough sigs to meet the threshold of requireSigs in the multiSignatureScriptPubKey")
      nullFailCheck(sigs, SignatureValidationErrorSignatureCount, flags)
    } else if (sigs.nonEmpty && pubKeys.nonEmpty) {
      val sig = sigs.head
      val pubKey = pubKeys.head
      val result = checkSignature(txSignatureComponent, script, pubKey, sig, flags)
      result match {
        case SignatureValidationSuccess =>
          multiSignatureEvaluator(txSignatureComponent, script, sigs.tail, pubKeys.tail, flags, requiredSigs - 1)
        case SignatureValidationErrorIncorrectSignatures | SignatureValidationErrorNullFail =>
          //notice we pattern match on 'SignatureValidationErrorNullFail' here, this is because
          //'checkSignature' may return that result, but we need to continue evaluating the signatures
          //in the multisig script, we don't check for nullfail until evaluation the OP_CHECKMULTSIG is completely done
          multiSignatureEvaluator(txSignatureComponent, script, sigs, pubKeys.tail, flags, requiredSigs)
        case x @ (SignatureValidationErrorNotStrictDerEncoding | SignatureValidationErrorSignatureCount |
          SignatureValidationErrorPubKeyEncoding | SignatureValidationErrorHighSValue |
          SignatureValidationErrorHashType | SignatureValidationErrorWitnessPubKeyType) =>
          nullFailCheck(sigs, x, flags)
      }
    } else if (sigs.isEmpty) {
      //means that we have checked all of the sigs against the public keys
      //validation succeeds
      SignatureValidationSuccess
    } else nullFailCheck(sigs, SignatureValidationErrorIncorrectSignatures, flags)

  }

  /**
   * If the NULLFAIL flag is set as defined in BIP146, it checks to make sure all failed signatures were an empty byte vector
   * [[https://github.com/bitcoin/bips/blob/master/bip-0146.mediawiki#NULLFAIL]]
   */
  private def nullFailCheck(sigs: Seq[ECDigitalSignature], result: TransactionSignatureCheckerResult, flags: Seq[ScriptFlag]): TransactionSignatureCheckerResult = {
    logger.info("Result before nullfail check:" + result)
    val nullFailEnabled = ScriptFlagUtil.requireScriptVerifyNullFail(flags)
    if (nullFailEnabled && !result.isValid && sigs.exists(_.bytes.nonEmpty)) {
      //we need to check that all signatures were empty byte vectors, else this fails because of BIP146 and nullfail
      SignatureValidationErrorNullFail
    } else result
  }

  /** Removes the hash type from the [[org.bitcoins.core.crypto.ECDigitalSignature]] */
  private def stripHashType(sig: ECDigitalSignature): ECDigitalSignature = {
    ECDigitalSignature(sig.bytes.slice(0, sig.bytes.length - 1))
  }
}

object TransactionSignatureChecker extends TransactionSignatureChecker

