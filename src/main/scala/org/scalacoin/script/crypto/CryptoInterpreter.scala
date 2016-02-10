package org.scalacoin.script.crypto

import org.scalacoin.protocol.script.ScriptPubKey
import org.scalacoin.protocol.transaction.Transaction
import org.scalacoin.script.{ScriptProgramFactory, ScriptProgramImpl, ScriptProgram}
import org.scalacoin.script.constant.{ScriptOperation, ScriptConstantImpl, ScriptConstant, ScriptToken}
import org.scalacoin.util.{CryptoUtil, ScalacoinUtil}


/**
 * Created by chris on 1/6/16.
 */
trait CryptoInterpreter extends ScalacoinUtil {

  /**
   * The input is hashed twice: first with SHA-256 and then with RIPEMD-160.
   * @param program
   * @return
   */
  def opHash160(program : ScriptProgram) : ScriptProgram = {
    require(program.stack.headOption.isDefined, "The top of the stack must be defined for OP_HASH160")
    require(program.script.headOption.isDefined && program.script.head == OP_HASH160, "Script operation must be OP_HASH160")
    val stackTop = program.stack.head
    val hash = ScriptConstantImpl(ScalacoinUtil.encodeHex(CryptoUtil.sha256Hash160(stackTop.bytes)))
    ScriptProgramFactory.factory(program, hash :: program.stack, program.script.tail)
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
    val newStackTop = ScriptConstantImpl(ScalacoinUtil.encodeHex(hash))
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
    val newStackTop = ScriptConstantImpl(ScalacoinUtil.encodeHex(hash))
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
    val newStackTop = ScriptConstantImpl(ScalacoinUtil.encodeHex(hash))
    ScriptProgramFactory.factory(program, newStackTop :: program.stack.tail, program.script.tail)
  }

  /**
   * The entire transaction's outputs, inputs, and script (from the most
   * recently-executed OP_CODESEPARATOR to the end) are hashed.
   * The signature used by OP_CHECKSIG must be a valid signature for this hash and public key.
   * If it is, 1 is returned, 0 otherwise.
   * @param inputScript
   * @param script
   * @return
   */
  def checkSig(inputScript : List[ScriptToken], script : List[ScriptToken], fullScript : List[ScriptToken]) : Boolean = {
    require(inputScript.size > 1, "We must have at least 2 inputs for our OP_CHECKSIG operation")
    require(script.headOption.isDefined && script.head == OP_CHECKSIG, "The top script stack element must be OP_CHECKSIG")
    val pubKey = inputScript.head
    val signature = inputScript(1)
    ???
  }



  /**
   * The entire transaction's outputs, inputs, and script (from the most
   * recently-executed OP_CODESEPARATOR to the end) are hashed.
   * The signature used by OP_CHECKSIG must be a valid signature for this hash and public key.
   * If it is, 1 is returned, 0 otherwise.
   * @param inputScript
   * @param script
   * @return
   */
  def checkSig(tx : Transaction, scriptPubKey : ScriptPubKey) : Boolean = {
    val inputIndex = 0
    val signature : ScriptToken = tx.inputs.head.scriptSignature.asm.head
    val pubKey : ScriptToken = tx.inputs.head.scriptSignature.asm(1)

    //delete ECDSA signature
    val inputWithoutScriptSig : Seq[ScriptToken] = tx.inputs.head.scriptSignature.asm.tail

    val fullScriptWithoutScripgSig : Seq[ScriptToken] = inputWithoutScriptSig ++ scriptPubKey.asm

    val hashTypeOpt : Option[HashType] = HashTypeFactory.fromByte(ScalacoinUtil.decodeHex(signature.hex).last)
    require(hashTypeOpt.isDefined, "We must have a hash type be the last byte on the given signature")
    val hashType = hashTypeOpt.get

    //hash for signature
    val hash : String = hashForSignature(inputWithoutScriptSig,tx,inputIndex,hashType)
    ???
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
    val hash = ScriptConstantImpl(ScalacoinUtil.encodeHex(CryptoUtil.sha1(constant.bytes)))
    ScriptProgramFactory.factory(program, hash :: program.stack.tail, program.script.tail)
  }

  def opCodeSeparator(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_CODESEPARATOR, "Script top must be OP_CODESEPARATOR")

    ScriptProgramFactory.factory(program,program.script.tail, ScriptProgramFactory.Script, 1)
  }

  private def hashForSignature(inputScript : Seq[ScriptToken], spendingTx : Transaction,
                            inputIndex : Int, hashType : HashType) : String = {
    require(inputIndex < spendingTx.inputs.size, "Given input index is out of range of the inputs in the spending tx")
    //Note: The transaction that uses SIGHASH_SINGLE type of signature should not have more inputs than outputs.
    //However if it does (because of the pre-existing implementation), it shall not be rejected,
    //but instead for every "illegal" input (meaning: an input that has an index bigger than the maximum output index)
    //the node should still verify it, though assuming the hash of
    val one = "0000000000000000000000000000000000000000000000000000000000000001"
    if(hashType == SIGHASH_SINGLE && inputIndex >= spendingTx.outputs.size) {
      one
    }

    ???

  }







}
