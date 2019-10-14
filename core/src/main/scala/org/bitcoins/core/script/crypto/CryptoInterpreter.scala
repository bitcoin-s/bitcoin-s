package org.bitcoins.core.script.crypto

import org.bitcoins.core.consensus.Consensus
import org.bitcoins.core.crypto._
import org.bitcoins.core.script.constant._
import org.bitcoins.core.script.control.{
  ControlOperationsInterpreter,
  OP_VERIFY
}
import org.bitcoins.core.script.flag.ScriptFlagUtil
import org.bitcoins.core.script.result._
import org.bitcoins.core.script._
import org.bitcoins.core.util.{BitcoinSLogger, BitcoinScriptUtil, CryptoUtil}
import scodec.bits.ByteVector

/**
  * Created by chris on 1/6/16.
  */
sealed abstract class CryptoInterpreter {

  private def logger = BitcoinSLogger.logger

  /** The input is hashed twice: first with SHA-256 and then with RIPEMD-160. */
  def opHash160(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_HASH160),
            "Script operation must be OP_HASH160")
    executeHashFunction(program, CryptoUtil.sha256Hash160(_: ByteVector))
  }

  /** The input is hashed using RIPEMD-160. */
  def opRipeMd160(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_RIPEMD160),
            "Script operation must be OP_RIPEMD160")
    executeHashFunction(program, CryptoUtil.ripeMd160(_: ByteVector))
  }

  /** The input is hashed using SHA-256. */
  def opSha256(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_SHA256),
            "Script operation must be OP_SHA256")
    executeHashFunction(program, CryptoUtil.sha256(_: ByteVector))
  }

  /** The input is hashed two times with SHA-256. */
  def opHash256(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_HASH256),
            "Script operation must be OP_HASH256")
    executeHashFunction(program, CryptoUtil.doubleSHA256(_: ByteVector))
  }

  /** The input is hashed using SHA-1. */
  def opSha1(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_SHA1),
            "Script top must be OP_SHA1")
    executeHashFunction(program, CryptoUtil.sha1(_: ByteVector))
  }

  /**
    * The entire transaction's outputs, inputs, and script (from the most
    * recently-executed OP_CODESEPARATOR to the end) are hashed.
    * The signature used by
    * [[org.bitcoins.core.script.crypto.OP_CHECKSIG OP_CHECKSIG]]
    * must be a valid signature for this hash and public key.
    * [[https://github.com/bitcoin/bitcoin/blob/528472111b4965b1a99c4bcf08ac5ec93d87f10f/src/script/interpreter.cpp#L880]]
    */
  def opCheckSig(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_CHECKSIG),
            "Script top must be OP_CHECKSIG")
    if (program.stack.size < 2) {
      logger.error("OP_CHECKSIG requires at lest two stack elements")
      program.failExecution(ScriptErrorInvalidStackOperation)
    } else {
      val pubKey = ECPublicKey(program.stack.head.bytes)
      val signature = ECDigitalSignature(program.stack.tail.head.bytes)
      val flags = program.flags
      val restOfStack = program.stack.tail.tail
      logger.debug(
        "Program before removing OP_CODESEPARATOR: " + program.originalScript)
      val removedOpCodeSeparatorsScript =
        BitcoinScriptUtil.removeOpCodeSeparator(program)
      logger.debug(
        "Program after removing OP_CODESEPARATOR: " + removedOpCodeSeparatorsScript)
      val result = TransactionSignatureChecker.checkSignature(
        program.txSignatureComponent,
        removedOpCodeSeparatorsScript,
        pubKey,
        signature,
        flags)
      handleSignatureValidation(program, result, restOfStack)
    }
  }

  /** Runs [[org.bitcoins.core.script.crypto.OP_CHECKSIG OP_CHECKSIG]] with an
    * [[org.bitcoins.core.script.control.OP_VERIFY OP_VERIFY]] afterwards. */
  def opCheckSigVerify(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_CHECKSIGVERIFY),
            "Script top must be OP_CHECKSIGVERIFY")
    if (program.stack.size < 2) {
      logger.error("Stack must contain at least 3 items for OP_CHECKSIGVERIFY")
      program.failExecution(ScriptErrorInvalidStackOperation)
    } else {
      val newScript = OP_CHECKSIG :: OP_VERIFY :: program.script.tail
      val newProgram = program.updateScript(newScript)
      val programFromOpCheckSig = opCheckSig(newProgram)
      logger.debug(
        "Stack after OP_CHECKSIG execution: " + programFromOpCheckSig.stack)
      programFromOpCheckSig match {
        case _: ExecutedScriptProgram =>
          programFromOpCheckSig
        case p: ExecutionInProgressScriptProgram =>
          ControlOperationsInterpreter.opVerify(p)
      }
    }
  }

  /**
    * All of the signature checking words will only match signatures to the data
    * after the most recently-executed
    * [[org.bitcoins.core.script.crypto.OP_CODESEPARATOR OP_CODESEPARATOR]].
    */
  def opCodeSeparator(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_CODESEPARATOR),
            "Script top must be OP_CODESEPARATOR")

    val indexOfOpCodeSeparator = program.originalScript.size - program.script.size

    program
      .updateScript(program.script.tail)
      .updateLastCodeSeparator(indexOfOpCodeSeparator)
  }

  /**
    * Compares the first signature against each public key until it finds an ECDSA match.
    * Starting with the subsequent public key, it compares the second signature against each remaining
    * public key until it finds an ECDSA match. The process is repeated until all signatures have been
    * checked or not enough public keys remain to produce a successful result.
    * All signatures need to match a public key.
    * Because public keys are not checked again if they fail any signature comparison,
    * signatures must be placed in the scriptSig using the same order as their corresponding public keys
    * were placed in the scriptPubKey or redeemScript. If all signatures are valid, 1 is returned, 0 otherwise.
    * Due to a bug, one extra unused value is removed from the stack.
    */
  final def opCheckMultiSig(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_CHECKMULTISIG),
            "Script top must be OP_CHECKMULTISIG")
    val flags = program.flags

    if (program.stack.size < 1) {
      logger.error("OP_CHECKMULTISIG requires at least 1 stack elements")
      program.failExecution(ScriptErrorInvalidStackOperation)
    } else {
      //these next lines remove the appropriate stack/script values after the signatures have been checked
      val nPossibleSignatures: ScriptNumber =
        BitcoinScriptUtil.numPossibleSignaturesOnStack(program)
      if (nPossibleSignatures < ScriptNumber.zero) {
        logger.error(
          "We cannot have the number of pubkeys in the script be negative")
        program.failExecution(ScriptErrorPubKeyCount)
      } else if (ScriptFlagUtil.requireMinimalData(flags) && !nPossibleSignatures.isShortestEncoding) {
        logger.error(
          "The required signatures and the possible signatures must be encoded as the shortest number possible")
        program.failExecution(ScriptErrorUnknownError)
      } else if (program.stack.size < 2) {
        logger.error("We need at least 2 operations on the stack")
        program.failExecution(ScriptErrorInvalidStackOperation)
      } else {
        val mRequiredSignatures: ScriptNumber =
          BitcoinScriptUtil.numRequiredSignaturesOnStack(program)

        if (ScriptFlagUtil.requireMinimalData(flags) && !mRequiredSignatures.isShortestEncoding) {
          logger.error(
            "The required signatures val must be the shortest encoding as possible")
          return program.failExecution(ScriptErrorUnknownError)
        }

        if (mRequiredSignatures < ScriptNumber.zero) {
          logger.error(
            "We cannot have the number of signatures specified in the script be negative")
          return program.failExecution(ScriptErrorSigCount)
        }
        logger.debug("nPossibleSignatures: " + nPossibleSignatures)
        val (pubKeysScriptTokens, stackWithoutPubKeys) =
          (program.stack.tail.slice(0, nPossibleSignatures.toInt),
           program.stack.tail
             .slice(nPossibleSignatures.toInt, program.stack.tail.size))

        val pubKeys = pubKeysScriptTokens.map(key => ECPublicKey(key.bytes))
        logger.debug("Public keys on the stack: " + pubKeys)
        logger.debug("Stack without pubkeys: " + stackWithoutPubKeys)
        logger.debug("mRequiredSignatures: " + mRequiredSignatures)

        //+1 is for the fact that we have the # of sigs + the script token indicating the # of sigs
        val signaturesScriptTokens = program.stack.tail.slice(
          nPossibleSignatures.toInt + 1,
          nPossibleSignatures.toInt + mRequiredSignatures.toInt + 1)
        val signatures =
          signaturesScriptTokens.map(token => ECDigitalSignature(token.bytes))
        logger.debug("Signatures on the stack: " + signatures)

        //this contains the extra Script OP that is required for OP_CHECKMULTISIG
        val stackWithoutPubKeysAndSignatures = stackWithoutPubKeys.tail
          .slice(mRequiredSignatures.toInt, stackWithoutPubKeys.tail.size)
        logger.debug(
          "stackWithoutPubKeysAndSignatures: " + stackWithoutPubKeysAndSignatures)
        if (pubKeys.size > Consensus.maxPublicKeysPerMultiSig) {
          logger.error(
            "We have more public keys than the maximum amount of public keys allowed")
          program.failExecution(ScriptErrorPubKeyCount)
        } else if (signatures.size > pubKeys.size) {
          logger.error(
            "We have more signatures than public keys inside OP_CHECKMULTISIG")
          program.failExecution(ScriptErrorSigCount)
        } else if (stackWithoutPubKeysAndSignatures.size < 1) {
          logger.error(
            "OP_CHECKMULTISIG must have a remaining element on the stack afterk execution")
          //this is because of a bug in bitcoin core for the implementation of OP_CHECKMULTISIG
          //https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.cpp#L966
          program.failExecution(ScriptErrorInvalidStackOperation)
        } else if (ScriptFlagUtil.requireNullDummy(flags) &&
                   (stackWithoutPubKeysAndSignatures.nonEmpty && stackWithoutPubKeysAndSignatures.head.bytes.nonEmpty)) {
          logger.error(
            "Script flag null dummy was set however the first element in the script signature was not an OP_0, stackWithoutPubKeysAndSignatures: " + stackWithoutPubKeysAndSignatures)
          program.failExecution(ScriptErrorSigNullDummy)
        } else {
          //remove the last OP_CODESEPARATOR
          val removedOpCodeSeparatorsScript =
            BitcoinScriptUtil.removeOpCodeSeparator(program)
          val isValidSignatures: TransactionSignatureCheckerResult =
            TransactionSignatureChecker.multiSignatureEvaluator(
              program.txSignatureComponent,
              removedOpCodeSeparatorsScript,
              signatures,
              pubKeys,
              flags,
              mRequiredSignatures.toLong)

          //remove the extra OP_0 (null dummy) for OP_CHECKMULTISIG from the stack
          val restOfStack = stackWithoutPubKeysAndSignatures.tail
          handleSignatureValidation(program, isValidSignatures, restOfStack)
        }
      }
    }
  }

  /** Runs
    * [[org.bitcoins.core.script.crypto.OP_CHECKMULTISIG OP_CHECKMULTISIG]] with an
    * [[org.bitcoins.core.script.control.OP_VERIFY OP_VERIFY]] afterwards */
  def opCheckMultiSigVerify(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_CHECKMULTISIGVERIFY),
            "Script top must be OP_CHECKMULTISIGVERIFY")
    if (program.stack.size < 3) {
      logger.error(
        "Stack must contain at least 3 items for OP_CHECKMULTISIGVERIFY")
      program.failExecution(ScriptErrorInvalidStackOperation)
    } else {
      val newScript = OP_CHECKMULTISIG :: OP_VERIFY :: program.script.tail
      val newProgram = program.updateScript(newScript)
      val programFromOpCheckMultiSig = opCheckMultiSig(newProgram)
      logger.debug(
        "Stack after OP_CHECKMULTSIG execution: " + programFromOpCheckMultiSig.stack)
      programFromOpCheckMultiSig match {
        case _: ExecutedScriptProgram =>
          programFromOpCheckMultiSig
        case p: ExecutionInProgressScriptProgram =>
          ControlOperationsInterpreter.opVerify(p)
      }
    }
  }

  /**
    * This is a higher order function designed to execute a hash function on the stack top of the program
    * For instance, we could pass in CryptoUtil.sha256 function as the `hashFunction` argument, which would then
    * apply sha256 to the stack top
    * @param program the script program whose stack top needs to be hashed
    * @param hashFunction the hash function which needs to be used on the stack top (sha256,ripemd160,etc..)
    * @return
    */
  private def executeHashFunction(
      program: ExecutionInProgressScriptProgram,
      hashFunction: ByteVector => HashDigest): StartedScriptProgram = {
    if (program.stack.nonEmpty) {
      val stackTop = program.stack.head
      val hash = ScriptConstant(hashFunction(stackTop.bytes).bytes)
      program.updateStackAndScript(hash :: program.stack.tail,
                                   program.script.tail)
    } else {
      logger.error(
        "We must have the stack top defined to execute a hash function")
      program.failExecution(ScriptErrorInvalidStackOperation)
    }
  }

  private def handleSignatureValidation(
      program: ExecutionInProgressScriptProgram,
      result: TransactionSignatureCheckerResult,
      restOfStack: Seq[ScriptToken]): StartedScriptProgram = result match {
    case SignatureValidationSuccess =>
      //means that all of the signatures were correctly encoded and
      //that all of the signatures were valid signatures for the given
      //public keys
      program.updateStackAndScript(OP_TRUE +: restOfStack, program.script.tail)
    case SignatureValidationErrorNotStrictDerEncoding =>
      //this means the script fails immediately
      //set the valid flag to false on the script
      //see BIP66 for more information on this
      //https://github.com/bitcoin/bips/blob/master/bip-0066.mediawiki#specification
      program.failExecution(ScriptErrorSigDer)
    case SignatureValidationErrorIncorrectSignatures =>
      //this means that signature verification failed, however all signatures were encoded correctly
      //just push a OP_FALSE onto the stack
      program.updateStackAndScript(OP_FALSE +: restOfStack, program.script.tail)
    case SignatureValidationErrorSignatureCount =>
      //means that we did not have enough signatures for OP_CHECKMULTISIG
      program.failExecution(ScriptErrorInvalidStackOperation)
    case SignatureValidationErrorPubKeyEncoding =>
      //means that a public key was not encoded correctly
      program.failExecution(ScriptErrorPubKeyType)
    case SignatureValidationErrorHighSValue =>
      program.failExecution(ScriptErrorSigHighS)
    case SignatureValidationErrorHashType =>
      program.failExecution(ScriptErrorSigHashType)
    case SignatureValidationErrorWitnessPubKeyType =>
      program.failExecution(ScriptErrorWitnessPubKeyType)
    case SignatureValidationErrorNullFail =>
      program.failExecution(ScriptErrorSigNullFail)
  }
}

object CryptoInterpreter extends CryptoInterpreter
