package org.scalacoin.util

import org.bitcoinj.core.Sha256Hash
import org.scalacoin.script.constant.{ScriptConstantImpl, ScriptConstant}

/**
 * Created by chris on 1/14/16.
 * Utility cryptographic functions
 */
trait CryptoUtil extends ScalacoinUtil {

  /**
   * Does the following computation
   * RIPEMD160(SHA256(hex))
   * @param hex
   * @return
   */
  def sha256Hash160(hex : String) : ScriptConstant = {
    val bytes = decodeHex(hex)
    val hash = org.bitcoinj.core.Utils.sha256hash160(bytes.toArray)
    ScriptConstantImpl(encodeHex(hash))

  }

  /**
   * Performs sha256(sha256(hex))
   * @param hex
   * @return
   */
  def doubleSHA256(hex : String) : String = {
    doubleSHA256(decodeHex(hex))
  }

  /**
   * Performs sha256(sha256(hex))
   * @param hex
   * @return
   */
  def doubleSHA256(bytes : List[Byte]) : String = {
    val hash : List[Byte] = Sha256Hash.hashTwice(bytes.toArray).toList
    encodeHex(hash.reverse)
  }
}

object CryptoUtil extends CryptoUtil
