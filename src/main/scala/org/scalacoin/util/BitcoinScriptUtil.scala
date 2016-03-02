package org.scalacoin.util

import org.scalacoin.script.constant.ScriptToken

/**
 * Created by chris on 3/2/16.
 */
trait BitcoinScriptUtil {

  /**
   * Takes in a sequence of script tokens and converts them to their hexadecimal value
   * @param asm
   * @return
   */
  def asmToHex(asm : Seq[ScriptToken]) : String = {
    val hex = asm.map(_.hex).mkString
    //the above hex val does NOT take into account the size of the entire asm script
    //we need to prepend the hex value of the size of the string
    BitcoinSUtil.longToHex(hex.size / 2) + hex
  }


  /**
   * Converts a sequence of script tokens to them to their byte values
   * @param asm
   * @return
   */
  def asmToBytes(asm : Seq[ScriptToken]) : Seq[Byte] = BitcoinSUtil.decodeHex(asmToHex(asm))
}


object BitcoinScriptUtil extends BitcoinScriptUtil
