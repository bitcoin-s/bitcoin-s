package org.scalacoin.script.constant

import org.scalacoin.util.{Factory, BitcoinSUtil}

/**
 * Created by chris on 2/26/16.
 */
trait ScriptConstantFactory extends Factory[ScriptConstant] {

  /**
   * Factory method for script constants
   * @param hex
   * @return
   */
  def factory(hex : String) : ScriptConstant = {
    ScriptConstantImpl(hex)
  }

  /**
   * Factory method for script constants
   * @param bytes
   * @return
   */
  def factory(bytes : Seq[Byte]) : ScriptConstant = {
    factory(BitcoinSUtil.encodeHex(bytes))
  }


  def fromBytes(bytes : Seq[Byte]) : ScriptConstant = factory(bytes)
}

object ScriptConstantFactory extends ScriptConstantFactory
