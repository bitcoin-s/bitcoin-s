package org.scalacoin.script.crypto

import org.scalacoin.script.ScriptOperation
import org.scalacoin.util.ScalacoinUtil


/**
 * Created by chris on 1/6/16.
 */
trait CryptoInterpreter extends ScalacoinUtil {

  def hash160(stack : List[String], script : List[ScriptOperation]) : (List[String], List[ScriptOperation]) = {
    require(stack.headOption.isDefined, "The top of the stack must be defined")
    require(script.headOption.isDefined && script.head == OP_HASH160, "Script operation must be OP_HASH160")
    val stackTop = stack.head
    val hash = sha256Hash160(stackTop)
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
/*
  def checkSig(inputScript : List[String], script : List[ScriptOperation]) : Boolean = {
    require(inputScript.size > 1, "We must have at least 2 inputs for our OP_CHECKSIG operation")
    require(script.headOption.isDefined && script.head == OP_CHECKSIG, "The top script stack element must be OP_CHECKSIG")
    val pubKey = inputScript.head
    val signature = inputScript(1)
    ???
  }
*/

  /*def codeSeparator()*/

  /**
   * Does the following computation
   * RIPEMD160(SHA256(hex))
   * @param hex
   * @return
   */
  private def sha256Hash160(hex : String) = {
    val bytes = decodeHex(hex)
    val hash = org.bitcoinj.core.Utils.sha256hash160(bytes)
    encodeHex(hash)

  }
}
