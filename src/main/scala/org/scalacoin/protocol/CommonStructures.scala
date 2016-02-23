package org.scalacoin.protocol

import org.scalacoin.util.ScalacoinUtil

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
   * The length of the VarInt in  bytes
   * @return
   */
  def size : Long

  def hex = size match {
    case 1 => num.toHexString
    case 3 => "fd" + ScalacoinUtil.littleEndianToBigEndian(ScalacoinUtil.longToHex(num))
    case 5 => "fe" + ScalacoinUtil.littleEndianToBigEndian(ScalacoinUtil.longToHex(num))
    case _ => "ff" + ScalacoinUtil.littleEndianToBigEndian(ScalacoinUtil.longToHex(num))
  }



}

case class CompactSizeUIntImpl(num : Long, size : Long) extends CompactSizeUInt



