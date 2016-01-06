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
