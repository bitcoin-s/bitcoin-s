package org.scalacoin.script.constant

import org.scalacoin.util.{Factory, BitcoinSUtil}

/**
 * Created by chris on 2/26/16.
 */
trait ScriptConstantFactory extends Factory[ScriptConstant] {
  /**
   * Creates a script constant from a sequence of bytes
   * @param bytes
   * @return
   */
  def fromBytes(bytes : Seq[Byte]) : ScriptConstant = ScriptConstantImpl(BitcoinSUtil.encodeHex(bytes))
}

object ScriptConstantFactory extends ScriptConstantFactory
