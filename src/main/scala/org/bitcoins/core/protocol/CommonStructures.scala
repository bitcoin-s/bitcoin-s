package org.bitcoins.core.protocol

import org.bitcoins.core.script.constant.ScriptNumberUtil
import org.bitcoins.core.util.BitcoinSUtil

/**
 * Created by chris on 7/14/15.
 */


trait CompactSizeUInt {

  /**
   * The number parsed from VarInt
 *
   * @return
   */
  def num : Long
  /**
   * The length of the VarInt in  bytes
 *
   * @return
   */
  def size : Long

  def hex = size match {
    case 1 => if (num.toHexString.size == 1) "0" + num.toHexString else num.toHexString
    case 3 => "fd" + ScriptNumberUtil.longToHex(num)
    case 5 => "fe" + ScriptNumberUtil.longToHex(num)
    case _ => "ff" + ScriptNumberUtil.longToHex(num)
  }



}

case class CompactSizeUIntImpl(num : Long, size : Long) extends CompactSizeUInt



