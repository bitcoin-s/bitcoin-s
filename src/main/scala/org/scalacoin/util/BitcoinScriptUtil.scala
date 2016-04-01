package org.scalacoin.util

import org.scalacoin.script.constant._

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
    hex
  }


  /**
   * Converts a sequence of script tokens to them to their byte values
   * @param asm
   * @return
   */
  def asmToBytes(asm : Seq[ScriptToken]) : Seq[Byte] = BitcoinSUtil.decodeHex(asmToHex(asm))

  /**
   * Filters out push operations in our scriptSig
   * this removes OP_PUSHDATA1, OP_PUSHDATA2, OP_PUSHDATA4 and all ByteToPushOntoStack tokens
   * @param asm
   * @return
   */
  def filterPushOps(asm : Seq[ScriptToken]) : Seq[ScriptToken] = {
    asm.filterNot(op => op.isInstanceOf[BytesToPushOntoStack]
      || op == OP_PUSHDATA1
      || op == OP_PUSHDATA2
      || op == OP_PUSHDATA4)
  }
}


object BitcoinScriptUtil extends BitcoinScriptUtil
