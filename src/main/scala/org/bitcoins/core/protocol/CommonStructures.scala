package org.bitcoins.core.protocol

import org.bitcoins.core.util.{BitcoinSUtil}

/**
 * Created by chris on 7/14/15.
 */


trait CompactSizeUInt {

  /**
   * The number parsed from VarInt
   * @return
   */
  def num : Long
  /**
   * The length of the VarInt in bytes
   * @return
   */
  def size : Long

  def hex = size match {
    case 1 => if (num.toHexString.size == 1) "0" + num.toHexString else num.toHexString
    case 3 => "fd" + BitcoinSUtil.longToHex(num)
    case 5 => "fe" + BitcoinSUtil.longToHex(num)
    case _ => "ff" + BitcoinSUtil.longToHex(num)
  }



}

object CompactSizeUInt {
  private sealed case class CompactSizeUIntImpl(num : Long, size : Long) extends CompactSizeUInt
  def apply(num : Long, size : Long) : CompactSizeUInt = {
    CompactSizeUIntImpl(num,size)
  }
}





