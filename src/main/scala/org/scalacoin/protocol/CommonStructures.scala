package org.scalacoin.protocol

import org.scalacoin.util.ScalacoinUtil

/**
 * Created by chris on 7/14/15.
 */


trait VarInt {

  /**
   * The number parsed from VarInt
   * @return
   */
  def num : Long
  /**
   * The length of the VarInt in  bytes
   * @return
   */
  def length : Long


}

case class VarIntImpl(num : Long, length : Long) extends VarInt



