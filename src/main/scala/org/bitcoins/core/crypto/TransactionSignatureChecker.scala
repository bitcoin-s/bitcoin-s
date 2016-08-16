package org.bitcoins.core.crypto

import org.bitcoins.core.config.TestNet3
import org.bitcoins.core.number.Int32
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionInput}
import org.bitcoins.core.script.ScriptProgram
import org.bitcoins.core.script.constant.{ScriptConstant, ScriptToken}
import org.bitcoins.core.script.crypto._
import org.bitcoins.core.script.flag.{ScriptFlag, ScriptFlagUtil, ScriptVerifyDerSig}
import org.bitcoins.core.util.{BitcoinSLogger, BitcoinSUtil, BitcoinScriptUtil}
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
    * @param txSignatureComponent the relevant transaction information for signature checking
    * @param script the current script state inside the interpreter - this is needed in the case of OP_CODESEPARATORS
    * @param pubKey the public key the signature is being checked against
    * @param signature the signature which is being checked against the transaction & the public key
    * @param flags the script flags used to check validity of the signature
    * @return a boolean indicating if the signature is valid or not
    */
  def checkSignature(txSignatureComponent : TransactionSignatureComponent, script : Seq[ScriptToken],
                     pubKey: ECPublicKey, signature : ECDigitalSignature, flags : Seq[ScriptFlag]) : TransactionSignatureCheckerResult = {
    logger.info("Signature: " + signature)
    val pubKeyEncodedCorrectly = BitcoinScriptUtil.checkPubKeyEncoding(pubKey,flags)
    if (ScriptFlagUtil.requiresStrictDerEncoding(flags) && !DERSignatureUtil.isValidSignatureEncoding(signature)) {
      logger.error("Signature was not stricly encoded der: " + signature.hex)
      SignatureValidationFailureNotStrictDerEncoding
    } else if (ScriptFlagUtil.requireLowSValue(flags) && !DERSignatureUtil.isLowS(signature)) {
      logger.error("Signature did not have a low s value")
      ScriptValidationFailureHighSValue
    } else if (ScriptFlagUtil.requireStrictEncoding(flags) && signature.bytes.nonEmpty &&
      !HashType.hashTypes.contains(HashType(signature.bytes.last))) {
      logger.error("signature: " + signature.bytes)
      logger.error("Hash type was not defined on the signature")
      ScriptValidationFailureHashType
    } else if (!pubKeyEncodedCorrectly) {
      logger.error("The public key given for signature checking was not encoded correctly")
      SignatureValidationFailurePubKeyEncoding
    } else {
      //we need to check if the scriptSignature has a redeemScript
      //in that case, we need to pass the redeemScript to the TransactionSignatureChecker
      //we do this by setting the scriptPubKey inside of txSignatureComponent to the redeemScript
      //instead of the p2sh scriptPubKey it was previously
      //as the scriptPubKey instead of the one inside of ScriptProgram
      val sigsRemovedScript : Seq[ScriptToken] = txSignatureComponent.scriptSignature match {
        case s : P2SHScriptSignature =>
          //needs to be here for removing all sigs from OP_CHECKMULTISIG
          //https://github.com/bitcoin/bitcoin/blob/master/src/test/data/tx_valid.json#L177
          //Finally CHECKMULTISIG removes all signatures prior to hashing the script containing those signatures.
          //In conjunction with the SIGHASH_SINGLE bug this lets us test whether or not FindAndDelete() is actually
          // present in scriptPubKey/redeemScript evaluation by including a signature of the digest 0x01
          // We can compute in advance for our pubkey, embed it it in the scriptPubKey, and then also
          // using a normal SIGHASH_ALL signature. If FindAndDelete() wasn't run, the 'bugged'
          //signature would still be in the hashed script, and the normal signature would fail."
          logger.info("Replacing redeemScript in txSignature component")
          logger.info("Redeem script: " + s.redeemScript)
          val sigsRemoved = removeSignaturesFromScript(s.signatures, s.redeemScript.asm)
          sigsRemoved
        case _ : P2PKHScriptSignature | _ : P2PKScriptSignature | _ : NonStandardScriptSignature
             | _ : MultiSignatureScriptSignature | EmptyScriptSignature =>
          logger.debug("Script before sigRemoved: "  + script)
          logger.debug("Signature: " + signature)
          logger.debug("PubKey: " + pubKey)
          val sigsRemoved = removeSignatureFromScript(signature,script)
          sigsRemoved
      }
      val hashTypeByte = if (signature.bytes.nonEmpty) signature.bytes.last else 0x00.toByte
      val hashType = HashType(Seq(0.toByte, 0.toByte, 0.toByte, hashTypeByte))
      val hashForSignature = TransactionSignatureSerializer.hashForSignature(txSignatureComponent.transaction,
        txSignatureComponent.inputIndex,
        sigsRemovedScript, hashType)
      logger.info("Hash for signature: " + BitcoinSUtil.encodeHex(hashForSignature.bytes))
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
   * @param script the script state this is needed in case there is an OP_CODESEPARATOR inside the script
   * @param sigs the signatures that are being checked for validity
   * @param pubKeys the public keys which are needed to verify that the signatures are correct
   * @param flags the script verify flags which are rules to verify the signatures
   * @return a boolean indicating if all of the signatures are valid against the given public keys
   */
  @tailrec
  final def multiSignatureEvaluator(txSignatureComponent : TransactionSignatureComponent, script : Seq[ScriptToken],
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
    else if (sigs.nonEmpty && pubKeys.nonEmpty) {
      val sig = sigs.head
      val pubKey = pubKeys.head
      val result = checkSignature(txSignatureComponent,script,pubKey,sig,flags)
      result match {
        case SignatureValidationSuccess =>
          multiSignatureEvaluator(txSignatureComponent, script, sigs.tail,pubKeys.tail,flags, requiredSigs - 1)
        case SignatureValidationFailureIncorrectSignatures =>
          multiSignatureEvaluator(txSignatureComponent, script, sigs, pubKeys.tail,flags, requiredSigs)
        case SignatureValidationFailureNotStrictDerEncoding =>
          SignatureValidationFailureNotStrictDerEncoding
        case SignatureValidationFailureSignatureCount =>
          SignatureValidationFailureSignatureCount
        case SignatureValidationFailurePubKeyEncoding =>
          SignatureValidationFailurePubKeyEncoding
        case ScriptValidationFailureHighSValue => ScriptValidationFailureHighSValue
        case ScriptValidationFailureHashType => ScriptValidationFailureHashType
      }
    } else if (sigs.isEmpty) {
      //means that we have checked all of the sigs against the public keys
      //validation succeeds
      SignatureValidationSuccess
    } else SignatureValidationFailureIncorrectSignatures
  }


  /**
    * Removes the given digital signature from the list of script tokens if it exists
 *
    * @param signature
    * @param script
    * @return
    */
  def removeSignatureFromScript(signature : ECDigitalSignature, script : Seq[ScriptToken]) : Seq[ScriptToken] = {
    if (script.contains(ScriptConstant(signature.hex))) {
      //replicates this line in bitcoin core
      //https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.cpp#L872
      val sigIndex = script.indexOf(ScriptConstant(signature.hex))
      logger.debug("SigIndex: " + sigIndex)
      //remove sig and it's corresponding BytesToPushOntoStack
      script.slice(0,sigIndex-1) ++ script.slice(sigIndex+1,script.size)
    } else script
  }

  /**
    * Removes the list of digital signatures from the list of script tokens
 *
    * @param sigs
    * @param script
    * @return
    */
  def removeSignaturesFromScript(sigs : Seq[ECDigitalSignature], script : Seq[ScriptToken]) : Seq[ScriptToken] = {
    @tailrec
    def loop(remainingSigs : Seq[ECDigitalSignature], scriptTokens : Seq[ScriptToken]) : Seq[ScriptToken] = {
      remainingSigs match {
        case Nil => scriptTokens
        case h :: t =>
          val newScriptTokens = removeSignatureFromScript(h,scriptTokens)
          loop(t,newScriptTokens)
      }
    }
    loop(sigs,script)
  }

}

object TransactionSignatureChecker extends TransactionSignatureChecker


