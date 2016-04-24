package org.scalacoin.script.constant

import org.scalacoin.util.{Factory, BitcoinSUtil}

/**
 * Created by chris on 2/26/16.
 */
trait ScriptConstantFactory extends Factory[ScriptConstant] {

  lazy val zero = ScriptConstantFactory.fromHex("00")
  lazy val negativeZero = ScriptConstantFactory.fromHex("80")
  lazy val negativeOne = ScriptConstantFactory.fromHex("81")
  /**
   * Creates a script constant from a sequence of bytes
   * @param bytes
   * @return
   */
  def fromBytes(bytes : Seq[Byte]) : ScriptConstant = ScriptConstantImpl(BitcoinSUtil.encodeHex(bytes))

}

object ScriptConstantFactory extends ScriptConstantFactory
