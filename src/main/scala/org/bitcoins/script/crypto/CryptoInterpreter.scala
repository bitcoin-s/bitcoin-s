package org.bitcoins.script.crypto

import org.bitcoins.crypto._
import org.bitcoins.protocol.script._
import org.bitcoins.protocol.transaction.Transaction
import org.bitcoins.script.control.{ControlOperationsInterpreter, OP_VERIFY}
import org.bitcoins.script.error._
import org.bitcoins.script.flag.{ScriptFlagUtil, ScriptVerifyNullDummy, ScriptVerifyDerSig}
import org.bitcoins.script._
import org.bitcoins.script.constant._
import org.bitcoins.util.{BitcoinScriptUtil, BitcoinSLogger, BitcoinSUtil, CryptoUtil}
import org.slf4j.LoggerFactory


/**
 * Created by chris on 1/6/16.
 */
trait CryptoInterpreter extends ControlOperationsInterpreter with BitcoinSLogger {

  /**
   * The input is hashed twice: first with SHA-256 and then with RIPEMD-160.
 *
   * @param program
   * @return
   */
  def opHash160(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_HASH160, "Script operation must be OP_HASH160")
    executeHashFunction(program, CryptoUtil.sha256Hash160(_ : List[Byte]))
  }


  /**
   * The input is hashed using RIPEMD-160.
 *
   * @param program
   * @return
   */
  def opRipeMd160(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_RIPEMD160, "Script operation must be OP_RIPEMD160")
    executeHashFunction(program, CryptoUtil.ripeMd160(_ : List[Byte]))
  }

  /**
   * The input is hashed using SHA-256.
 *
   * @param program
   * @return
   */
  def opSha256(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_SHA256, "Script operation must be OP_SHA256")
    executeHashFunction(program, CryptoUtil.sha256(_ : List[Byte]))
  }

  /**
   * The input is hashed two times with SHA-256.
 *
   * @param program
   * @return
   */
  def opHash256(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_HASH256, "Script operation must be OP_HASH256")
    executeHashFunction(program, CryptoUtil.doubleSHA256(_ : List[Byte]))
  }

  /**
   * The input is hashed using SHA-1.
 *
   * @param program
   * @return
   */
  def opSha1(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_SHA1, "Script top must be OP_SHA1")
    executeHashFunction(program, CryptoUtil.sha1(_ : List[Byte]))
  }

  /**
   * The entire transaction's outputs, inputs, and script (from the most
   * recently-executed OP_CODESEPARATOR to the end) are hashed.
   * The signature used by OP_CHECKSIG must be a valid signature for this hash and public key.
   * If it is, 1 is returned, 0 otherwise.
   * https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.cpp#L818
 *
   * @param program
   * @return
   */
  def opCheckSig(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_CHECKSIG, "Script top must be OP_CHECKSIG")
    if (program.stack.size < 2) {
      logger.error("OP_CHECKSIG requires at lest two stack elements")
      ScriptProgram(program,ScriptErrorInvalidStackOperation)
    } else {
      val pubKey = ECFactory.publicKey(program.stack.head.bytes)
      val signature = ECFactory.digitalSignature(program.stack.tail.head.bytes)

      if (ScriptFlagUtil.requiresStrictDerEncoding(program.flags) && !DERSignatureUtil.isStrictDEREncoding(signature)) {
        //this means all of the signatures must encoded according to BIP66 strict dersig
        //https://github.com/bitcoin/bips/blob/master/bip-0066.mediawiki
        //script verification fails since the sig is not strictly der encoded
        logger.warn("Since the ScriptVerifyDerSig flag is set the signature being checked must be a strict dersig signature as per BIP 66\n" +
          "Sig: " + signature.hex)
        ScriptProgram(program,ScriptErrorSigDer)
      } else {
        val restOfStack = program.stack.tail.tail
        val result = TransactionSignatureChecker.checkSignature(program.txSignatureComponent,pubKey,
          signature,program.flags)
        logger.debug("signature verification isValid: " + result)
        result match {
          case SignatureValidationSuccess => ScriptProgram(program,
            OP_TRUE :: restOfStack,program.script.tail)
          case SignatureValidationFailureNotStrictDerEncoding =>
            ScriptProgram(program, ScriptErrorSigDer)
          case SignatureValidationFailureIncorrectSignatures =>
            ScriptProgram(program, OP_FALSE :: restOfStack,program.script.tail)
          case SignatureValidationFailureSignatureCount =>
            ScriptProgram(program, OP_FALSE :: restOfStack,program.script.tail)
          case SignatureValidationFailurePubKeyEncoding =>
            //means that a public key was not encoded correctly
            ScriptProgram(program,ScriptErrorPubKeyType)
        }
      }
    }


  }




  /**
   * All of the signature checking words will only match signatures to the data
   * after the most recently-executed OP_CODESEPARATOR.
 *
   * @param program
   * @return
   */
  def opCodeSeparator(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_CODESEPARATOR, "Script top must be OP_CODESEPARATOR")
    val fullScript = program.txSignatureComponent.scriptSignature.asm.containsSlice(program.script) match {
      case true => program.txSignatureComponent.scriptSignature.asm
      case false => program.txSignatureComponent.scriptPubKey.asm
    }
    val indexOfOpCodeSeparator = fullScript.indexOf(OP_CODESEPARATOR)
    require(indexOfOpCodeSeparator != -1,"The script we searched MUST contain an OP_CODESEPARTOR. Script: " + fullScript)
    val e = program match {
      case e : ExecutionInProgressScriptProgram => e
    }
    ScriptProgram(e,program.script.tail,ScriptProgram.Script,indexOfOpCodeSeparator)
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
 *
   * @param program
   * @return
   */
  def opCheckMultiSig(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_CHECKMULTISIG, "Script top must be OP_CHECKMULTISIG")

    if (program.flags.contains(ScriptVerifyNullDummy) && program.txSignatureComponent.scriptSignature.asm.head != OP_0) {
      logger.warn("Script flag null dummy was set however the first element in the script signature was not an OP_0")
      ScriptProgram(program,ScriptErrorSigNullDummy)
    } else if (program.stack.size < 3) {
      logger.error("OP_CHECKMULTISIG requires at least 3 stack elements")
      ScriptProgram(program,ScriptErrorInvalidStackOperation)
    } else {
      //these next lines remove the appropriate stack/script values after the signatures have been checked
      val nPossibleSignatures : ScriptNumber  = BitcoinScriptUtil.numPossibleSignaturesOnStack(program)
      val mRequiredSignatures : ScriptNumber = BitcoinScriptUtil.numRequiredSignaturesOnStack(program)

      if (ScriptFlagUtil.requireMinimalData(program.flags) && (!nPossibleSignatures.isShortestEncoding
        || !mRequiredSignatures.isShortestEncoding)) {
        logger.error("The required signatures and the possible signatures must be encoded as the shortest number possible")
        ScriptProgram(program, ScriptErrorMinimalData)
      } else {
        logger.debug("nPossibleSignatures: " + nPossibleSignatures)
        val (pubKeysScriptTokens,stackWithoutPubKeys) =
          (program.stack.tail.slice(0,nPossibleSignatures.num.toInt),
            program.stack.tail.slice(nPossibleSignatures.num.toInt,program.stack.tail.size))

        val pubKeys = pubKeysScriptTokens.map(key => ECFactory.publicKey(key.bytes))
        logger.debug("Public keys on the stack: " + pubKeys)

        logger.debug("mRequiredSignatures: " + mRequiredSignatures)

        //+1 is for the fact that we have the # of sigs + the script token indicating the # of sigs
        val signaturesScriptTokens = program.stack.tail.slice(nPossibleSignatures.num.toInt + 1,
          nPossibleSignatures.num.toInt + mRequiredSignatures.num.toInt + 1)
        val signatures = signaturesScriptTokens.map(token => ECFactory.digitalSignature(token.bytes))
        logger.debug("Signatures on the stack: " + signatures)
        logger.debug("ScriptPubKey: " + program.txSignatureComponent.scriptPubKey)
        //+1 is for bug in OP_CHECKMULTSIG that requires an extra OP to be pushed onto the stack
        val stackWithoutPubKeysAndSignatures = stackWithoutPubKeys.tail.slice(mRequiredSignatures.num.toInt+1, stackWithoutPubKeys.tail.size)
        val restOfStack = stackWithoutPubKeysAndSignatures


        if (pubKeys.size > ScriptSettings.maxPublicKeysPerMultiSig) {
          logger.error("We have more public keys than the maximum amount of public keys allowed")
          ScriptProgram(program,ScriptErrorPubKeyCount)
        }
        else if (signatures.size > pubKeys.size) {
          logger.error("We have more signatures than public keys inside OP_CHECKMULTISIG")
          ScriptProgram(program, ScriptErrorSigCount)
        } else {
          val isValidSignatures : TransactionSignatureCheckerResult =
            TransactionSignatureChecker.multiSignatureEvaluator(program.txSignatureComponent,signatures,
              pubKeys,program.flags,mRequiredSignatures.num)

          isValidSignatures match {
            case SignatureValidationSuccess =>
              //means that all of the signatures were correctly encoded and
              //that all of the signatures were valid signatures for the given
              //public keys
              ScriptProgram(program, OP_TRUE :: restOfStack, program.script.tail)
            case SignatureValidationFailureNotStrictDerEncoding =>
              //this means the script fails immediately
              //set the valid flag to false on the script
              //see BIP66 for more information on this
              //https://github.com/bitcoin/bips/blob/master/bip-0066.mediawiki#specification
              ScriptProgram(program, ScriptErrorSigDer)
            case SignatureValidationFailureIncorrectSignatures =>
              //this means that signature verification failed, however all signatures were encoded correctly
              //just push a ScriptFalse onto the stack
              ScriptProgram(program, OP_FALSE :: restOfStack, program.script.tail)
            case SignatureValidationFailureSignatureCount =>
              //means that we did not have enough signatures for OP_CHECKMULTISIG
              ScriptProgram(program, ScriptErrorSigCount)
            case SignatureValidationFailurePubKeyEncoding =>
              //means that a public key was not encoded correctly
              ScriptProgram(program,ScriptErrorPubKeyType)
          }
        }
      }




    }
  }


  /**
   * Runs OP_CHECKMULTISIG with an OP_VERIFY afterwards
 *
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
 *
   * @param program the script program whose stack top needs to be hashed
   * @param hashFunction the hash function which needs to be used on the stack top (sha256,ripemd160,etc..)
   * @return
   */
  private def executeHashFunction(program : ScriptProgram, hashFunction : List[Byte] => List[Byte]) : ScriptProgram = {
    if (program.stack.headOption.isDefined) {
      val stackTop = program.stack.head
      val hash = ScriptConstant(hashFunction(stackTop.bytes))
      ScriptProgram(program, hash :: program.stack.tail, program.script.tail)
    } else {
      logger.error("We must have the stack top defined to execute a hash function")
      ScriptProgram(program,ScriptErrorInvalidStackOperation)
    }
  }

}
