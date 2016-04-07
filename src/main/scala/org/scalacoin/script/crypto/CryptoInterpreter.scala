package org.scalacoin.script.crypto

import org.scalacoin.crypto._
import org.scalacoin.protocol.script._
import org.scalacoin.protocol.transaction.Transaction
import org.scalacoin.script.control.{ControlOperationsInterpreter, OP_VERIFY}
import org.scalacoin.script.flag.{ScriptVerifyNullDummy, ScriptVerifyDerSig}
import org.scalacoin.script.{ScriptProgramFactory, ScriptProgram}
import org.scalacoin.script.constant._
import org.scalacoin.util.{BitcoinSLogger, BitcoinSUtil, CryptoUtil}
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
    require(program.stack.headOption.isDefined, "The top of the stack must be defined for OP_HASH160")
    require(program.script.headOption.isDefined && program.script.head == OP_HASH160, "Script operation must be OP_HASH160")
    val stackTop = program.stack.head
    val hash = ScriptConstantFactory.fromBytes(CryptoUtil.sha256Hash160(stackTop.bytes))
    ScriptProgramFactory.factory(program, hash :: program.stack.tail, program.script.tail)
  }


  /**
   * The input is hashed using RIPEMD-160.
   * @param program
   * @return
   */
  def opRipeMd160(program : ScriptProgram) : ScriptProgram = {
    require(program.stack.headOption.isDefined, "The top of the stack must be defined for OP_RIPEMD160")
    require(program.script.headOption.isDefined && program.script.head == OP_RIPEMD160, "Script operation must be OP_RIPEMD160")
    val stackTop = program.stack.head
    val hash = CryptoUtil.ripeMd160(stackTop.bytes)
    val newStackTop = ScriptConstantFactory.fromBytes((hash))
    ScriptProgramFactory.factory(program,newStackTop :: program.stack.tail, program.script.tail)
  }

  /**
   * The input is hashed using SHA-256.
   * @param program
   * @return
   */
  def opSha256(program : ScriptProgram) : ScriptProgram = {
    require(program.stack.headOption.isDefined, "The top of the stack must be defined for OP_SHA256")
    require(program.script.headOption.isDefined && program.script.head == OP_SHA256, "Script operation must be OP_SHA256")
    val stackTop = program.stack.head
    val hash = CryptoUtil.sha256(stackTop.bytes)
    val newStackTop = ScriptConstantFactory.fromBytes(hash)
    ScriptProgramFactory.factory(program, newStackTop :: program.stack.tail, program.script.tail)
  }

  /**
   * The input is hashed two times with SHA-256.
   * @param program
   * @return
   */
  def opHash256(program : ScriptProgram) : ScriptProgram = {
    require(program.stack.headOption.isDefined, "The top of the stack must be defined for OP_HASH256")
    require(program.script.headOption.isDefined && program.script.head == OP_HASH256, "Script operation must be OP_HASH256")
    val stackTop = program.stack.head
    val hash = CryptoUtil.doubleSHA256(stackTop.bytes)
    val newStackTop = ScriptConstantFactory.fromBytes(hash)
    ScriptProgramFactory.factory(program, newStackTop :: program.stack.tail, program.script.tail)
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
    require(program.stack.size > 1, "Stack must have at least 2 items on it for OP_CHECKSIG")


    val pubKey = ECFactory.publicKey(program.stack.head.bytes)
    val signature = ECFactory.digitalSignature(program.stack.tail.head.bytes)

    if (program.flags.contains(ScriptVerifyDerSig) && !DERSignatureUtil.isStrictDEREncoding(signature)) {
      //this means all of the signatures must encoded according to BIP66 strict dersig
      //https://github.com/bitcoin/bips/blob/master/bip-0066.mediawiki
      //script verification fails since the sig is not strictly der encoded
      logger.warn("Since the ScriptVerifyDerSig flag is set the signature being checked must be a strict dersig signature as per BIP 66\n" +
        "Sig: " + signature.hex)
      ScriptProgramFactory.factory(program,false)
    } else {
      val restOfStack = program.stack.tail.tail
      //we need to check if the scriptSignature has a redeemScript
      //in that case, we need to pass the redeemScript to the TransactionSignatureChecker
      //as the scriptPubKey instead of the one inside of ScriptProgram
      val scriptPubKey = program.txSignatureComponent.scriptSignature match {
        case s : P2SHScriptSignature => s.redeemScript
        case _ : P2PKHScriptSignature | _ : P2PKScriptSignature | _ : NonStandardScriptSignature
             | _ : MultiSignatureScriptSignature | EmptyScriptSignature => program.txSignatureComponent.scriptPubKey
      }
      val result = TransactionSignatureChecker.checkSignature(program.txSignatureComponent.transaction,
        program.txSignatureComponent.inputIndex,scriptPubKey,pubKey,signature,program.flags.contains(ScriptVerifyDerSig))
      logger.debug("signature verification isValid: " + result)
      result match {
        case SignatureValidationSuccess => ScriptProgramFactory.factory(program, ScriptTrue :: restOfStack,program.script.tail)
        case SignatureValidationFailureNotStrictDerEncoding =>
          ScriptProgramFactory.factory(program, ScriptFalse :: restOfStack,program.script.tail,SignatureValidationFailureNotStrictDerEncoding.isValid)
        case SignatureValidationFailureIncorrectSignatures =>
          ScriptProgramFactory.factory(program, ScriptFalse :: restOfStack,program.script.tail)
      }
    }
  }


  /**
   * The input is hashed using SHA-1.
   * @param program
   * @return
   */
  def opSha1(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_SHA1, "Script top must be OP_SHA1")
    require(program.stack.headOption.isDefined, "We must have an element on the stack for OP_SHA1")

    val constant = program.stack.head
    val hash = ScriptConstantFactory.fromBytes(CryptoUtil.sha1(constant.bytes))
    ScriptProgramFactory.factory(program, hash :: program.stack.tail, program.script.tail)
  }

  /**
   * All of the signature checking words will only match signatures to the data
   * after the most recently-executed OP_CODESEPARATOR.
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
    ScriptProgramFactory.factory(program,program.script.tail,ScriptProgramFactory.Script,indexOfOpCodeSeparator)
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
    require(program.stack.size > 2, "Stack must contain at least 3 items for OP_CHECKMULTISIG")

    if (program.flags.contains(ScriptVerifyNullDummy) && program.txSignatureComponent.scriptSignature.asm.head != OP_0) {
      logger.warn("Script flag null dummy was set however the first element in the script signature was not an OP_0")
      ScriptProgramFactory.factory(program,false)
    } else {
      //these next lines remove the appropriate stack/script values after the signatures have been checked
      val nPossibleSignatures : Int  = program.stack.head match {
        case s : ScriptNumber => s.num.toInt
        case _ => throw new RuntimeException("n must be a script number for OP_CHECKMULTISIG")
      }
      logger.debug("nPossibleSignatures: " + nPossibleSignatures)
      val (pubKeysScriptTokens,stackWithoutPubKeys) =
        (program.stack.tail.slice(0,nPossibleSignatures),program.stack.tail.slice(nPossibleSignatures,program.stack.tail.size))
      val pubKeys = pubKeysScriptTokens.map(key => ECFactory.publicKey(key.hex))
      logger.debug("Public keys on the stack: " + pubKeys)
      val mRequiredSignatures : Int = stackWithoutPubKeys.head match {
        case s: ScriptNumber => s.num.toInt
        case _ => throw new RuntimeException("m must be a script number for OP_CHECKMULTISIG")
      }
      logger.debug("mRequiredSignatures: " + mRequiredSignatures )

      //+1 is for the fact that we have the # of sigs + the script token indicating the # of sigs
      val signaturesScriptTokens = program.stack.tail.slice(nPossibleSignatures + 1, nPossibleSignatures + mRequiredSignatures + 1)
      val signatures = signaturesScriptTokens.map(token => ECFactory.digitalSignature(token.bytes))
      logger.debug("Signatures on the stack: " + signatures)
      logger.debug("ScriptPubKey: " + program.txSignatureComponent.scriptPubKey)
      //+1 is for bug in OP_CHECKMULTSIG that requires an extra OP to be pushed onto the stack
      val stackWithoutPubKeysAndSignatures = stackWithoutPubKeys.tail.slice(mRequiredSignatures+1, stackWithoutPubKeys.tail.size)
      val restOfStack = stackWithoutPubKeysAndSignatures


      val isValidSignatures : TransactionSignatureCheckerResult =
        TransactionSignatureChecker.multiSignatureEvaluator(program.txSignatureComponent,signatures,
          pubKeys,program.flags,mRequiredSignatures)

      isValidSignatures match {
        case SignatureValidationSuccess =>
          //means that all of the signatures were correctly encoded and
          //that all of the signatures were valid signatures for the given
          //public keys
          ScriptProgramFactory.factory(program, ScriptTrue :: restOfStack, program.script.tail)
        case SignatureValidationFailureNotStrictDerEncoding =>
          //this means the script fails immediately
          //set the valid flag to false on the script
          //see BIP66 for more information on this
          //https://github.com/bitcoin/bips/blob/master/bip-0066.mediawiki#specification
          ScriptProgramFactory.factory(program, restOfStack, program.script.tail,false)
        case SignatureValidationFailureIncorrectSignatures =>
          //this means that signature verification failed, however all signatures were encoded correctly
          //just push a ScriptFalse onto the stack
          ScriptProgramFactory.factory(program, ScriptFalse :: restOfStack, program.script.tail)
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
    require(program.stack.size > 2, "Stack must contain at least 3 items for OP_CHECKMULTISIGVERIFY")
    val newScript = OP_CHECKMULTISIG :: OP_VERIFY :: program.script.tail
    val newProgram = ScriptProgramFactory.factory(program,newScript, ScriptProgramFactory.Script)
    val programFromOpCheckMultiSig = opCheckMultiSig(newProgram)
    logger.debug("Stack after OP_CHECKMULTSIG execution: " + programFromOpCheckMultiSig.stack)
    val programFromOpVerify = opVerify(programFromOpCheckMultiSig)
    programFromOpVerify
  }

}
