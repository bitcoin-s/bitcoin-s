package org.scalacoin.script.crypto

import org.scalacoin.protocol.script.ScriptPubKey
import org.scalacoin.protocol.transaction.Transaction
import org.scalacoin.script.constant.{ScriptOperation, ScriptConstantImpl, ScriptConstant, ScriptToken}
import org.scalacoin.util.{CryptoUtil, ScalacoinUtil}


/**
 * Created by chris on 1/6/16.
 */
trait CryptoInterpreter extends ScalacoinUtil {

  def hash160(stack : List[ScriptToken], script : List[ScriptToken]) : (List[ScriptToken], List[ScriptToken]) = {
    require(stack.headOption.isDefined, "The top of the stack must be defined")
    require(script.headOption.isDefined && script.head == OP_HASH160, "Script operation must be OP_HASH160")
    val stackTop = stack.head
    val hash = stackTop match {
      case ScriptConstantImpl(x) => CryptoUtil.sha256Hash160(x)
      case _ => throw new RuntimeException("Stack top should be of type ScriptConstant to call hash160 on it")
    }
    (hash :: stack, script.tail)
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
    val signature : ScriptToken = tx.inputs.head.scriptSignature.asm.head
    val pubKey : ScriptToken = tx.inputs.head.scriptSignature.asm(1)

    //delete ECDSA signature
    val inputWithoutScriptSig : Seq[ScriptToken] = tx.inputs.head.scriptSignature.asm.tail

    val fullScriptWithoutScripgSig : Seq[ScriptToken] = inputWithoutScriptSig ++ scriptPubKey.asm

    //val hashType = HashTypeFactory.factory(ScalacoinUtil.decodeHex(signature.hex).last)
    //check signature against the tx

    ???
  }







}
