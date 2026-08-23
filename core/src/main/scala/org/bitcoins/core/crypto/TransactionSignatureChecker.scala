package org.bitcoins.core.crypto

import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script.*
import org.bitcoins.core.protocol.transaction.TransactionOutput
import org.bitcoins.core.script.constant.ScriptToken
import org.bitcoins.core.script.flag.{ScriptFlag, ScriptFlagUtil}
import org.bitcoins.core.script.result.*
import org.bitcoins.core.util.BitcoinScriptUtil
import org.bitcoins.crypto.*
import scodec.bits.ByteVector

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

/** Created by chris on 2/16/16. Responsible for checking digital signatures on
  * inputs against their respective public keys
  */
trait TransactionSignatureChecker {

  def checkSignature(
      txSignatureComponent: TxSigComponent,
      pubKeyBytes: ECPublicKeyBytes,
      signature: ECDigitalSignature): TransactionSignatureCheckerResult =
    checkSignature(txSignatureComponent = txSignatureComponent,
                   script =
                     txSignatureComponent.fundingOutput.scriptPubKey.asm.toList,
                   pubKey = pubKeyBytes,
                   signature = signature)

  def checkSignature(
      txSignatureComponent: TxSigComponent,
      pubKey: ECPublicKey,
      signature: ECDigitalSignature): TransactionSignatureCheckerResult = {
    checkSignature(
      txSignatureComponent = txSignatureComponent,
      script = txSignatureComponent.fundingOutput.scriptPubKey.asm.toList,
      pubKey = pubKey.toPublicKeyBytes(),
      signature = signature
    )
  }

  /** @param txSigComponent
    * @param schnorrSignature
    * @param pubKey
    * @see
    *   https://github.com/bitcoin/bitcoin/blob/8ae4ba481ce8f7da173bef24432729c87a36cb70/src/script/interpreter.cpp#L1695
    * @return
    */
  def checkSchnorrSignature(
      txSigComponent: TxSigComponent,
      pubKey: SchnorrPublicKey,
      witness: TaprootKeyPath,
      taprootOptions: TaprootSerializationOptions): ScriptResult = {
    if (witness.signature.hashTypeOpt.contains(HashType.sigHashDefault)) {
      // cannot have DEFAULT hash type explicitly defined with BIP341
      ScriptErrorSchnorrSigHashType
    } else {
      checkSchnorrSignature(txSigComponent = txSigComponent,
                            pubKey = pubKey,
                            schnorrSignature = witness.signature,
                            taprootOptions)
    }
  }

  def checkSchnorrSignature(
      txSigComponent: TxSigComponent,
      pubKey: SchnorrPublicKey,
      schnorrSignature: SchnorrDigitalSignature,
      taprootOptions: TaprootSerializationOptions): ScriptResult = {
    require(
      txSigComponent.sigVersion == SigVersionTaprootKeySpend
        || txSigComponent.sigVersion == SigVersionTapscript,
      s"SigVerison must be Taproot or Tapscript, got=${txSigComponent.sigVersion}"
    )

    val hashType =
      schnorrSignature.hashTypeOpt.getOrElse(HashType.sigHashDefault)
    // bip341 restricts valid hash types: https://github.com/bitcoin/bips/blob/master/bip-0341.mediawiki#common-signature-message
    val validHashType = HashType.checkTaprootHashType(hashType)
    if (!validHashType) {
      ScriptErrorSchnorrSigHashType
    } else {
      // hashForSignature throws for a taproot SIGHASH_SINGLE with no
      // corresponding output (BIP341 requires that to fail validation,
      // unlike the legacy sighash algorithm's placeholder error hash) --
      // that must surface as a clean script failure here, not an uncaught
      // exception escaping the interpreter
      Try(
        TransactionSignatureSerializer.hashForSignature(txSigComponent,
                                                        hashType,
                                                        taprootOptions)
      ) match {
        case Success(hash) =>
          val result = pubKey.verify(hash, schnorrSignature)
          if (result) ScriptOk else ScriptErrorSchnorrSig
        case Failure(_) =>
          ScriptErrorSchnorrSig
      }
    }
  }

  /** Checks the signature of a scriptSig in the spending transaction against
    * the given scriptPubKey & explicitly given public key This is useful for
    * instances of non standard scriptSigs
    *
    * @param txSignatureComponent
    *   the relevant transaction information for signature checking
    * @param script
    *   the current script state inside the interpreter - this is needed in the
    *   case of OP_CODESEPARATORS
    * @param pubKey
    *   the public key the signature is being checked against
    * @param signature
    *   the signature which is being checked against the transaction & the
    *   public key
    * @param flags
    *   the script flags used to check validity of the signature
    * @return
    *   a boolean indicating if the signature is valid or not
    */
  def checkSignature(
      txSignatureComponent: TxSigComponent,
      script: Seq[ScriptToken],
      pubKey: ECPublicKeyBytes,
      signature: ECDigitalSignature,
      flags: Seq[ScriptFlag] = Policy.standardFlags)
      : TransactionSignatureCheckerResult = {
    txSignatureComponent.sigVersion match {
      case SigVersionTapscript | SigVersionTaprootKeySpend =>
        sys.error(
          s"Call checkTapScript signature to validate a tapscript signature")
      case SigVersionWitnessV0 | SigVersionBase =>
        val pubKeyEncodedCorrectly = BitcoinScriptUtil.isValidPubKeyEncoding(
          pubKey,
          txSignatureComponent.sigVersion,
          flags)
        if (
          ScriptFlagUtil.requiresStrictDerEncoding(flags) && !DERSignatureUtil
            .isValidSignatureEncoding(signature)
        ) {
          SignatureValidationErrorNotStrictDerEncoding
        } else if (
          ScriptFlagUtil.requireLowSValue(
            flags) && signature.bytes.nonEmpty && !DERSignatureUtil
            .isLowS(signature)
        ) {
          SignatureValidationErrorHighSValue
        } else if (
          ScriptFlagUtil.requireStrictEncoding(
            flags) && signature.bytes.nonEmpty &&
          !HashType.isDefinedHashtypeSignature(signature)
        ) {
          SignatureValidationErrorHashType
        } else if (pubKeyEncodedCorrectly.isDefined) {
          val err = pubKeyEncodedCorrectly.get
          val result =
            if (err == ScriptErrorWitnessPubKeyType)
              SignatureValidationErrorWitnessPubKeyType
            else SignatureValidationErrorPubKeyEncoding
          result
        } else {
          val sigsRemovedScript = BitcoinScriptUtil.calculateScriptForChecking(
            txSignatureComponent,
            signature,
            script)
          val hashTypeByte =
            if (signature.bytes.nonEmpty) signature.bytes.last else 0x00.toByte
          val hashType = HashType(
            ByteVector(0.toByte, 0.toByte, 0.toByte, hashTypeByte))
          val spk = ScriptPubKey.fromAsm(sigsRemovedScript)
          val hashForSignature = txSignatureComponent match {
            case w: WitnessTxSigComponentRaw =>
              // BIP143 commits to the scriptCode after the last executed
              // OP_CODESEPARATOR (sigsRemovedScript/spk above), not the raw
              // witness script -- rebuild the component with the stripped
              // script so the sighash actually commits to it, mirroring the
              // BaseTxSigComponent/WitnessTxSigComponentRebuilt cases below
              val sigsRemovedTxSigComponent = WitnessTxSigComponentRebuilt(
                wtx = w.transaction,
                inputIndex = w.inputIndex,
                output = TransactionOutput(w.fundingOutput.value, spk),
                witScriptPubKey = w.scriptPubKey,
                flags = w.flags)
              TransactionSignatureSerializer.hashForSignature(
                sigsRemovedTxSigComponent,
                hashType,
                TaprootSerializationOptions.empty)
            case w: WitnessTxSigComponent =>
              TransactionSignatureSerializer.hashForSignature(
                w,
                hashType,
                TaprootSerializationOptions.empty)
            case b: BaseTxSigComponent =>
              val sigsRemovedTxSigComponent = BaseTxSigComponent(
                b.transaction,
                b.inputIndex,
                TransactionOutput(b.fundingOutput.value, spk),
                b.flags)
              TransactionSignatureSerializer.hashForSignature(
                sigsRemovedTxSigComponent,
                hashType,
                TaprootSerializationOptions.empty)
            case r: WitnessTxSigComponentRebuilt =>
              val sigsRemovedTxSigComponent = WitnessTxSigComponentRebuilt(
                wtx = r.transaction,
                inputIndex = r.inputIndex,
                output = TransactionOutput(r.fundingOutput.value, spk),
                witScriptPubKey = r.witnessScriptPubKey,
                flags = r.flags)
              TransactionSignatureSerializer.hashForSignature(
                sigsRemovedTxSigComponent,
                hashType,
                TaprootSerializationOptions.empty)
          }

          val sigWithoutHashType = stripHashType(signature)
          val isValid = pubKey.verify(hashForSignature, sigWithoutHashType)
          if (isValid) SignatureValidationSuccess
          else
            nullFailCheck(Seq(signature),
                          SignatureValidationErrorIncorrectSignatures,
                          flags)
        }
    }
  }

  def checkSigTapscript(
      txSignatureComponent: TxSigComponent,
      pubKey: SchnorrPublicKey,
      signature: SchnorrDigitalSignature,
      hashType: HashType,
      taprootOptions: TaprootSerializationOptions,
      flags: Seq[ScriptFlag]): TransactionSignatureCheckerResult = {
    // hashForSignature throws for a taproot SIGHASH_SINGLE with no
    // corresponding output (BIP341 requires that to fail validation,
    // unlike the legacy sighash algorithm's placeholder error hash) --
    // that must surface as a clean signature check failure here, not an
    // uncaught exception escaping the interpreter
    Try(
      TransactionSignatureSerializer.hashForSignature(txSignatureComponent,
                                                      hashType,
                                                      taprootOptions)
    ) match {
      case Failure(_) =>
        tapscriptNullFailCheck(signature)
      case Success(hash) =>
        val result = pubKey.verify(hash, signature)
        if (result) {
          SignatureValidationSuccess
        } else {
          tapscriptNullFailCheck(signature)
        }
    }
  }

  /** This is a helper function to check digital signatures against public keys
    * if the signature does not match this public key, check it against the next
    * public key in the sequence
    * @param txSignatureComponent
    *   the tx signature component that contains all relevant transaction
    *   information
    * @param script
    *   the script state this is needed in case there is an OP_CODESEPARATOR
    *   inside the script
    * @param sigs
    *   the signatures that are being checked for validity
    * @param pubKeys
    *   the public keys which are needed to verify that the signatures are
    *   correct
    * @param flags
    *   the script verify flags which are rules to verify the signatures
    * @param originalSigs
    *   the full signature list as originally provided to OP_CHECKMULTISIG,
    *   before any were consumed off the front of `sigs` by a successful match
    *   -- needed for the NULLFAIL check below. Callers making the initial
    *   (non-recursive) call should pass the same list as `sigs` (or
    *   `List.empty` if there are no signatures to check).
    * @return
    *   a boolean indicating if all of the signatures are valid against the
    *   given public keys
    */
  @tailrec
  final def multiSignatureEvaluator(
      txSignatureComponent: TxSigComponent,
      script: Seq[ScriptToken],
      sigs: List[ECDigitalSignature],
      pubKeys: List[ECPublicKeyBytes],
      flags: Seq[ScriptFlag],
      requiredSigs: Long,
      originalSigs: List[ECDigitalSignature])
      : TransactionSignatureCheckerResult = {
    require(requiredSigs >= 0,
            s"requiredSigs cannot be negative, got $requiredSigs")
    // NULLFAIL (BIP146) applies to every signature originally provided to
    // OP_CHECKMULTISIG, not just the ones remaining at the point validation
    // is determined to have failed -- a signature that matched earlier and
    // was "consumed" (sigs.tail'd away) must still be checked.
    if (sigs.size > pubKeys.size) {
      // this is how bitcoin core treats this. If there are ever any more
      // signatures than public keys remaining we immediately return
      // false https://github.com/bitcoin/bitcoin/blob/8c1dbc5e9ddbafb77e60e8c4e6eb275a3a76ac12/src/script/interpreter.cpp#L943-L945
      nullFailCheck(originalSigs,
                    SignatureValidationErrorIncorrectSignatures,
                    flags)
    } else if (requiredSigs > sigs.size) {
      // for the case when we do not have enough sigs left to check to meet the required signature threshold
      // https://github.com/bitcoin/bitcoin/blob/8c1dbc5e9ddbafb77e60e8c4e6eb275a3a76ac12/src/script/interpreter.cpp#L990-L991
      nullFailCheck(originalSigs, SignatureValidationErrorSignatureCount, flags)
    } else if (sigs.nonEmpty && pubKeys.nonEmpty) {
      val sig = sigs.head
      val pubKey = pubKeys.head
      val result =
        checkSignature(txSignatureComponent, script, pubKey, sig, flags)
      result match {
        case SignatureValidationSuccess =>
          multiSignatureEvaluator(txSignatureComponent,
                                  script,
                                  sigs.tail,
                                  pubKeys.tail,
                                  flags,
                                  requiredSigs - 1,
                                  originalSigs)
        case SignatureValidationErrorIncorrectSignatures |
            SignatureValidationErrorNullFail =>
          // notice we pattern match on 'SignatureValidationErrorNullFail' here, this is because
          // 'checkSignature' may return that result, but we need to continue evaluating the signatures
          // in the multisig script, we don't check for nullfail until evaluation the OP_CHECKMULTSIG is completely done
          multiSignatureEvaluator(txSignatureComponent,
                                  script,
                                  sigs,
                                  pubKeys.tail,
                                  flags,
                                  requiredSigs,
                                  originalSigs)
        case x @ (SignatureValidationErrorNotStrictDerEncoding |
            SignatureValidationErrorSignatureCount |
            SignatureValidationErrorPubKeyEncoding |
            SignatureValidationErrorHighSValue |
            SignatureValidationErrorHashType |
            SignatureValidationErrorWitnessPubKeyType) =>
          nullFailCheck(originalSigs, x, flags)
      }
    } else if (sigs.isEmpty) {
      // means that we have checked all of the sigs against the public keys
      // validation succeeds
      SignatureValidationSuccess
    } else
      nullFailCheck(originalSigs,
                    SignatureValidationErrorIncorrectSignatures,
                    flags)

  }

  /** If the NULLFAIL flag is set as defined in BIP146, it checks to make sure
    * all failed signatures were an empty byte vector
    * [[https://github.com/bitcoin/bips/blob/master/bip-0146.mediawiki#NULLFAIL]]
    */
  private def nullFailCheck(
      sigs: Seq[ECDigitalSignature],
      result: TransactionSignatureCheckerResult,
      flags: Seq[ScriptFlag]): TransactionSignatureCheckerResult = {
    val nullFailEnabled = ScriptFlagUtil.requireScriptVerifyNullFail(flags)
    if (nullFailEnabled && !result.isValid && sigs.exists(_.bytes.nonEmpty)) {
      // we need to check that all signatures were empty byte vectors, else this fails because of BIP146 and nullfail
      SignatureValidationErrorNullFail
    } else result
  }

  /** BIP342 makes NULLFAIL mandatory for tapscript signature checks, regardless
    * of whether the SCRIPT_VERIFY_NULLFAIL flag is set: an invalid non-empty
    * signature must always fail the script immediately, not push false and
    * continue like the legacy/segwit v0 OP_CHECKSIG "soft fail" behavior does.
    * An empty signature is a valid way to signal "no signature provided" and
    * continues to soft-fail (push false).
    */
  private def tapscriptNullFailCheck(
      signature: SchnorrDigitalSignature): TransactionSignatureCheckerResult = {
    if (signature.bytes.nonEmpty) {
      SignatureValidationErrorNullFail
    } else {
      SignatureValidationErrorIncorrectSignatures
    }
  }

  /** Removes the hash type from the [[ECDigitalSignature]] */
  private def stripHashType(sig: ECDigitalSignature): ECDigitalSignature = {
    ECDigitalSignature(sig.bytes.slice(0, sig.bytes.length - 1))
  }
}

object TransactionSignatureChecker extends TransactionSignatureChecker
