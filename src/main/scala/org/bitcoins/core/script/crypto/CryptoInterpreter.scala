package org.bitcoins.core.script.crypto

import org.bitcoins.core.crypto.{SignatureValidationFailureIncorrectSignatures, _}
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.script.control.{ControlOperationsInterpreter, OP_VERIFY}
import org.bitcoins.core.script.result._
import org.bitcoins.core.script.flag.{ScriptFlagUtil, ScriptVerifyDerSig, ScriptVerifyNullDummy}
import org.bitcoins.core.script._
import org.bitcoins.core.script.constant._
import org.bitcoins.core.util.{BitcoinSLogger, BitcoinSUtil, BitcoinScriptUtil, CryptoUtil}
import org.slf4j.LoggerFactory


/**
 * Created by chris on 1/6/16.
 */
trait CryptoInterpreter extends ControlOperationsInterpreter with BitcoinSLogger {

  /**
   * The input is hashed twice: first with SHA-256 and then with RIPEMD-160.
   * @param program
   * @return
   */
  def opHash160(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_HASH160, "Script operation must be OP_HASH160")
    executeHashFunction(program, CryptoUtil.sha256Hash160(_ : Seq[Byte]))
  }


  /**
   * The input is hashed using RIPEMD-160.
   * @param program
   * @return
   */
  def opRipeMd160(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_RIPEMD160, "Script operation must be OP_RIPEMD160")
    executeHashFunction(program, CryptoUtil.ripeMd160(_ : Seq[Byte]))
  }

  /**
   * The input is hashed using SHA-256.
   * @param program
   * @return
   */
  def opSha256(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_SHA256, "Script operation must be OP_SHA256")
    executeHashFunction(program, CryptoUtil.sha256(_ : Seq[Byte]))
  }

  /**
   * The input is hashed two times with SHA-256.
   * @param program
   * @return
   */
  def opHash256(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_HASH256, "Script operation must be OP_HASH256")
    executeHashFunction(program, CryptoUtil.doubleSHA256(_ : Seq[Byte]))
  }

  /**
   * The input is hashed using SHA-1.
   * @param program
   * @return
   */
  def opSha1(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_SHA1, "Script top must be OP_SHA1")
    executeHashFunction(program, CryptoUtil.sha1(_ : Seq[Byte]))
  }

  /**
   * The entire transaction's outputs, inputs, and script (from the most
   * recently-executed OP_CODESEPARATOR to the end) are hashed.
   * The signature used by OP_CHECKSIG must be a valid signature for this hash and public key.
   * If it is, 1 is returned, 0 otherwise.
   * https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.cpp#L818
   * @param program
   * @return
   */
  def opCheckSig(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_CHECKSIG, "Script top must be OP_CHECKSIG")
    program match {
      case preExecutionScriptProgram : PreExecutionScriptProgram =>
        opCheckSig(ScriptProgram.toExecutionInProgress(preExecutionScriptProgram))
      case executedScriptprogram : ExecutedScriptProgram =>
        executedScriptprogram
      case executionInProgressScriptProgram : ExecutionInProgressScriptProgram =>
        if (executionInProgressScriptProgram.stack.size < 2) {
          logger.error("OP_CHECKSIG requires at lest two stack elements")
          ScriptProgram(program,ScriptErrorInvalidStackOperation)
        } else {
          val pubKey = ECPublicKey(executionInProgressScriptProgram.stack.head.bytes)
          val signature = ECDigitalSignature(executionInProgressScriptProgram.stack.tail.head.bytes)

          if (ScriptFlagUtil.requiresStrictDerEncoding(executionInProgressScriptProgram.flags) &&
            !DERSignatureUtil.isValidSignatureEncoding(signature)) {
            //this means all of the signatures must encoded according to BIP66 strict dersig
            //https://github.com/bitcoin/bips/blob/master/bip-0066.mediawiki
            //script verification fails since the sig is not strictly der encoded
            logger.error("Since the ScriptVerifyDerSig flag is set the signature being checked must be a strict dersig signature as per BIP 66\n" +
              "Sig: " + signature.hex)
            ScriptProgram(executionInProgressScriptProgram,ScriptErrorSigDer)
          } else {
            val restOfStack = executionInProgressScriptProgram.stack.tail.tail
            logger.debug("Program before removing OP_CODESEPARATOR: " + program.originalScript)
            val removedOpCodeSeparatorsScript = removeOpCodeSeparator(executionInProgressScriptProgram)
            logger.debug("Program after removing OP_CODESEPARATOR: " + removedOpCodeSeparatorsScript)
            val result = TransactionSignatureChecker.checkSignature(executionInProgressScriptProgram.txSignatureComponent,
              removedOpCodeSeparatorsScript, pubKey, signature, program.flags)
            logger.debug("signature verification isValid: " + result)
            result match {
              case SignatureValidationSuccess => ScriptProgram(program,
                OP_TRUE :: restOfStack,program.script.tail)
              case SignatureValidationFailureNotStrictDerEncoding =>
                logger.info("Signature validation failed: " + SignatureValidationFailureNotStrictDerEncoding)
                ScriptProgram(program, ScriptErrorSigDer)
              case SignatureValidationFailureIncorrectSignatures =>
                logger.info("Signature validation failed: " + SignatureValidationFailureIncorrectSignatures)
                ScriptProgram(program, OP_FALSE :: restOfStack,program.script.tail)
              case SignatureValidationFailureSignatureCount =>
                logger.info("Signature validation failed: " + SignatureValidationFailureSignatureCount)
                ScriptProgram(program, OP_FALSE :: restOfStack,program.script.tail)
              case SignatureValidationFailurePubKeyEncoding =>
                logger.info("Signature validation failed: " + SignatureValidationFailurePubKeyEncoding)
                //means that a public key was not encoded correctly
                ScriptProgram(program,ScriptErrorPubKeyType)
              case ScriptValidationFailureHighSValue =>
                logger.info("Signature validation failed: " + ScriptValidationFailureHighSValue)
                ScriptProgram(program,ScriptErrorSigHighS)
              case ScriptValidationFailureHashType =>
                logger.info("Signature validation failed: " + ScriptValidationFailureHashType)
                ScriptProgram(program,ScriptErrorSigHashType)
            }
          }
        }
    }
  }

  /**
    * Runs OP_CHECKSIG with an OP_VERIFY afterwards
    * @param program
    * @return
    */
  def opCheckSigVerify(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_CHECKSIGVERIFY,
      "Script top must be OP_CHECKSIGVERIFY")
    if (program.stack.size < 2) {
      logger.error("Stack must contain at least 3 items for OP_CHECKSIGVERIFY")
      ScriptProgram(program,ScriptErrorInvalidStackOperation)
    } else {
      val newScript = OP_CHECKSIG :: OP_VERIFY :: program.script.tail
      val newProgram = ScriptProgram(program,newScript, ScriptProgram.Script)
      val programFromOpCheckSig = opCheckSig(newProgram)
      logger.debug("Stack after OP_CHECKSIG execution: " + programFromOpCheckSig.stack)
      programFromOpCheckSig match {
        case _ : PreExecutionScriptProgram | _ : ExecutedScriptProgram =>
          programFromOpCheckSig
        case _ : ExecutionInProgressScriptProgram => opVerify(programFromOpCheckSig)
      }
    }
  }
  
  /**
   * All of the signature checking words will only match signatures to the data
   * after the most recently-executed OP_CODESEPARATOR.
   * @param program
   * @return
   */
  def opCodeSeparator(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_CODESEPARATOR, "Script top must be OP_CODESEPARATOR")
    val e = program match {
      case e : PreExecutionScriptProgram =>
        opCodeSeparator(ScriptProgram.toExecutionInProgress(e))
      case e : ExecutionInProgressScriptProgram =>
        val indexOfOpCodeSeparator = program.originalScript.size - program.script.size
        ScriptProgram(e,program.script.tail,ScriptProgram.Script,indexOfOpCodeSeparator)
      case e : ExecutedScriptProgram =>
        ScriptProgram(e,ScriptErrorUnknownError)
    }
    e
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
    * @param program
    * @return
    */
  def opCheckMultiSig(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_CHECKMULTISIG, "Script top must be OP_CHECKMULTISIG")

    program match {
      case preExecutionScriptProgram : PreExecutionScriptProgram =>
        opCheckMultiSig(ScriptProgram.toExecutionInProgress(preExecutionScriptProgram))
      case executedScriptProgram : ExecutedScriptProgram =>
        executedScriptProgram
      case executionInProgressScriptProgram : ExecutionInProgressScriptProgram =>
        if (program.stack.size < 1) {
          logger.error("OP_CHECKMULTISIG requires at least 1 stack elements")
          ScriptProgram(executionInProgressScriptProgram,ScriptErrorInvalidStackOperation)
        } else {
          //these next lines remove the appropriate stack/script values after the signatures have been checked
          val nPossibleSignatures : ScriptNumber = BitcoinScriptUtil.numPossibleSignaturesOnStack(program)
          if (nPossibleSignatures < ScriptNumber.zero) {
            logger.error("We cannot have the number of pubkeys in the script be negative")
            ScriptProgram(program,ScriptErrorPubKeyCount)
          } else if (ScriptFlagUtil.requireMinimalData(program.flags) && !nPossibleSignatures.isShortestEncoding) {
            logger.error("The required signatures and the possible signatures must be encoded as the shortest number possible")
            ScriptProgram(executionInProgressScriptProgram, ScriptErrorUnknownError)
          } else if (program.stack.size < 2) {
            logger.error("We need at least 2 operations on the stack")
            ScriptProgram(executionInProgressScriptProgram,ScriptErrorInvalidStackOperation)
          } else {
            val mRequiredSignatures: ScriptNumber = BitcoinScriptUtil.numRequiredSignaturesOnStack(program)

            if (ScriptFlagUtil.requireMinimalData(program.flags) && !mRequiredSignatures.isShortestEncoding) {
              logger.error("The required signatures val must be the shortest encoding as possible")
              return ScriptProgram(executionInProgressScriptProgram, ScriptErrorUnknownError)
            }

            if (mRequiredSignatures < ScriptNumber.zero) {
              logger.error("We cannot have the number of signatures specified in the script be negative")
              return ScriptProgram(executionInProgressScriptProgram, ScriptErrorSigCount)
            }
            logger.debug("nPossibleSignatures: " + nPossibleSignatures)
            val (pubKeysScriptTokens, stackWithoutPubKeys) =
              (program.stack.tail.slice(0, nPossibleSignatures.toInt),
                program.stack.tail.slice(nPossibleSignatures.toInt, program.stack.tail.size))

            val pubKeys = pubKeysScriptTokens.map(key => ECPublicKey(key.bytes))
            logger.debug("Public keys on the stack: " + pubKeys)
            logger.debug("Stack without pubkeys: " + stackWithoutPubKeys)
            logger.debug("mRequiredSignatures: " + mRequiredSignatures)

            //+1 is for the fact that we have the # of sigs + the script token indicating the # of sigs
            val signaturesScriptTokens = program.stack.tail.slice(nPossibleSignatures.toInt + 1,
              nPossibleSignatures.toInt + mRequiredSignatures.toInt + 1)
            val signatures = signaturesScriptTokens.map(token => ECDigitalSignature(token.bytes))
            logger.debug("Signatures on the stack: " + signatures)

            //this contains the extra Script OP that is required for OP_CHECKMULTISIG
            val stackWithoutPubKeysAndSignatures = stackWithoutPubKeys.tail.slice(mRequiredSignatures.toInt, stackWithoutPubKeys.tail.size)
            logger.debug("stackWithoutPubKeysAndSignatures: " + stackWithoutPubKeysAndSignatures)
            if (pubKeys.size > ScriptSettings.maxPublicKeysPerMultiSig) {
              logger.error("We have more public keys than the maximum amount of public keys allowed")
              ScriptProgram(executionInProgressScriptProgram, ScriptErrorPubKeyCount)
            } else if (signatures.size > pubKeys.size) {
              logger.error("We have more signatures than public keys inside OP_CHECKMULTISIG")
              ScriptProgram(executionInProgressScriptProgram, ScriptErrorSigCount)
            } else if (stackWithoutPubKeysAndSignatures.size < 1) {
              logger.error("OP_CHECKMULTISIG must have a remaining element on the stack afterk execution")
              //this is because of a bug in bitcoin core for the implementation of OP_CHECKMULTISIG
              //https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.cpp#L966
              ScriptProgram(executionInProgressScriptProgram,ScriptErrorInvalidStackOperation)
            } else if (ScriptFlagUtil.requireNullDummy(program.flags) &&
              !(Seq(Some(OP_0), Some(ScriptNumber.zero)).contains(stackWithoutPubKeysAndSignatures.headOption))) {
              logger.error("Script flag null dummy was set however the first element in the script signature was not an OP_0, stackWithoutPubKeysAndSignatures: " + stackWithoutPubKeysAndSignatures)
              ScriptProgram(executionInProgressScriptProgram,ScriptErrorSigNullDummy)
            } else {
              //remove the last OP_CODESEPARATOR
              val removedOpCodeSeparatorsScript = removeOpCodeSeparator(executionInProgressScriptProgram)
              val isValidSignatures: TransactionSignatureCheckerResult =
                TransactionSignatureChecker.multiSignatureEvaluator(executionInProgressScriptProgram.txSignatureComponent,
                  removedOpCodeSeparatorsScript, signatures,
                  pubKeys, program.flags, mRequiredSignatures.underlying)

              //remove the extra op for OP_CHECKMULTISIG from the stack
              val restOfStack = stackWithoutPubKeysAndSignatures.tail
              isValidSignatures match {
                case SignatureValidationSuccess =>
                  //means that all of the signatures were correctly encoded and
                  //that all of the signatures were valid signatures for the given
                  //public keys
                  ScriptProgram(executionInProgressScriptProgram, OP_TRUE :: restOfStack, program.script.tail)
                case SignatureValidationFailureNotStrictDerEncoding =>
                  //this means the script fails immediately
                  //set the valid flag to false on the script
                  //see BIP66 for more information on this
                  //https://github.com/bitcoin/bips/blob/master/bip-0066.mediawiki#specification
                  ScriptProgram(executionInProgressScriptProgram, ScriptErrorSigDer)
                case SignatureValidationFailureIncorrectSignatures =>
                  //this means that signature verification failed, however all signatures were encoded correctly
                  //just push a ScriptFalse onto the stack
                  ScriptProgram(executionInProgressScriptProgram, OP_FALSE :: restOfStack, program.script.tail)
                case SignatureValidationFailureSignatureCount =>
                  //means that we did not have enough signatures for OP_CHECKMULTISIG
                  ScriptProgram(executionInProgressScriptProgram, ScriptErrorInvalidStackOperation)
                case SignatureValidationFailurePubKeyEncoding =>
                  //means that a public key was not encoded correctly
                  ScriptProgram(executionInProgressScriptProgram, ScriptErrorPubKeyType)
                case ScriptValidationFailureHighSValue =>
                  ScriptProgram(program, ScriptErrorSigHighS)
                case ScriptValidationFailureHashType =>
                  ScriptProgram(executionInProgressScriptProgram, ScriptErrorSigHashType)
              }
            }
          }
        }
      }
    }


  /**
   * Runs OP_CHECKMULTISIG with an OP_VERIFY afterwards
   * @param program
   * @return
   */
  def opCheckMultiSigVerify(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_CHECKMULTISIGVERIFY, "Script top must be OP_CHECKMULTISIGVERIFY")
    if (program.stack.size < 3) {
      logger.error("Stack must contain at least 3 items for OP_CHECKMULTISIGVERIFY")
      ScriptProgram(program,ScriptErrorInvalidStackOperation)
    } else {
      val newScript = OP_CHECKMULTISIG :: OP_VERIFY :: program.script.tail
      val newProgram = ScriptProgram(program,newScript, ScriptProgram.Script)
      val programFromOpCheckMultiSig = opCheckMultiSig(newProgram)
      logger.debug("Stack after OP_CHECKMULTSIG execution: " + programFromOpCheckMultiSig.stack)
      programFromOpCheckMultiSig match {
        case _ : PreExecutionScriptProgram | _ : ExecutedScriptProgram =>
          programFromOpCheckMultiSig
        case _ : ExecutionInProgressScriptProgram => opVerify(programFromOpCheckMultiSig)
      }
    }
  }


  /**
   * This is a higher order function designed to execute a hash function on the stack top of the program
   * For instance, we could pass in CryptoUtil.sha256 function as the 'hashFunction' argument, which would then
   * apply sha256 to the stack top
   * @param program the script program whose stack top needs to be hashed
   * @param hashFunction the hash function which needs to be used on the stack top (sha256,ripemd160,etc..)
   * @return
   */
  private def executeHashFunction(program : ScriptProgram, hashFunction : Seq[Byte] => HashDigest) : ScriptProgram = {
    if (program.stack.headOption.isDefined) {
      val stackTop = program.stack.head
      val hash = ScriptConstant(hashFunction(stackTop.bytes).bytes)
      ScriptProgram(program, hash :: program.stack.tail, program.script.tail)
    } else {
      logger.error("We must have the stack top defined to execute a hash function")
      ScriptProgram(program,ScriptErrorInvalidStackOperation)
    }
  }


  /**
    * Removes the OP_CODESEPARATOR in the original script according to
    * the last code separator index in the script
    * @param program
    * @return
    */
  def removeOpCodeSeparator(program : ExecutionInProgressScriptProgram) : Seq[ScriptToken] = {
    if (program.lastCodeSeparator.isDefined) {
      program.originalScript.slice(program.lastCodeSeparator.get+1, program.originalScript.size)
    } else program.originalScript
  }

}
