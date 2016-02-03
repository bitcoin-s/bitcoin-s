package org.bitcoins.protocol

import org.bitcoins.util.ScalacoinUtil

/**
 * Created by chris on 7/14/15.
 */


trait VarInt {
  def length : BigInt
}
/**
 * Variable length integer
 * Integer can be encoded depending on the represented value to save space.
 * Variable length integers always precede an array/vector of a type of data that may vary in length.
 * Longer numbers are encoded in little endian.
  *
  * @param serialization
 */
case class NetworkVarInt( serialization : String) extends VarInt with ScalacoinUtil {
  override def length : BigInt = {
    val slice = hexToBigInt(serialization.slice(0,2))
    if (slice == 0xFD) hexToBigInt(serialization.slice(2,6))
    else if (slice == 0xFE) hexToBigInt(serialization.slice(2,10))
    else if (slice == 0xFF) hexToBigInt(serialization.slice(2,18))
    else hexToBigInt(serialization.slice(0,2))
  }

}


