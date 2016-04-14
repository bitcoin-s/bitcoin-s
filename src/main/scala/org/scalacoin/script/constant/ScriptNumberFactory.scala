package org.scalacoin.script.constant

import org.scalacoin.util.{BitcoinSUtil, Factory}

/**
 * Created by chris on 4/4/16.
 */
trait ScriptNumberFactory extends Factory[ScriptNumber] {

  /**
   * Bitcoin has a numbering system which has a negative zero
   */
  lazy val negativeZero : ScriptNumber = fromHex("80")
  /**
   * Represents the number zero inside of bitcoin's script language
   * @return
   */
  lazy val zero : ScriptNumber = OP_0
  /**
   * Represents the number one inside of bitcoin's script language
   * @return
   */
  lazy val one : ScriptNumber = fromNumber(1)


  def fromBytes(bytes : Seq[Byte]) : ScriptNumber = {
    if (bytes.isEmpty || bytes == Seq(0x0)) zero
    else ScriptNumberImpl(bytes)
  }



  /**
   * Creates a script number from a scala number.
   * This requires the endianess to be flipped on the Scala number since bitcoin uses big endian encoding
   * @param num
   * @return
   */
  def fromNumber(num : Long) : ScriptNumber = fromHex(BitcoinSUtil.longToHex(num))
}

object ScriptNumberFactory extends ScriptNumberFactory
