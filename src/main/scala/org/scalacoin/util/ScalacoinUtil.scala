package org.scalacoin.util

import org.bitcoinj.core.{Base58, Utils}

/**
 * Created by chris on 7/26/15.
 */
trait ScalacoinUtil {

  def hexToBigInt(hex : String) : BigInt = BigInt(hex, 16)

  def decodeHex( hex : String) : Array[Byte] = Utils.HEX.decode(hex)

  def encodeHex(bytes : Array[Byte]) : String = Utils.HEX.encode(bytes)

  def decodeBase58(base58 : String) : Array[Byte] = Base58.decode(base58)
}

object ScalacoinUtil extends ScalacoinUtil