package org.scalacoin.util

import org.bitcoinj.core.{Base58, Utils}

/**
 * Created by chris on 7/26/15.
 */
trait ScalacoinUtil {

  def hexToBigInt(hex : String) : BigInt = BigInt(hex, 16)

  def decodeHex(hex : String) : List[Byte] = Utils.HEX.decode(hex).toList

  def encodeHex(bytes : Array[Byte]) : String = Utils.HEX.encode(bytes)

  def encodeHex(bytes : List[Byte]) : String = encodeHex(bytes.toArray)

  def encodeHex(byte : Byte ) : String = Utils.HEX.encode(Array(byte))

  def decodeBase58(base58 : String) : List[Byte] = Base58.decode(base58).toList
}

object ScalacoinUtil extends ScalacoinUtil