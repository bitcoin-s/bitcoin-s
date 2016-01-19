package org.scalacoin.protocol.script

import org.scalacoin.marshallers.script.ScriptParser
import org.scalacoin.script.constant.ScriptToken
import org.scalacoin.util.ScalacoinUtil

/**
 * Created by chris on 1/19/16.
 * Responsible for the instantiation of ScriptSignature objects
 */
object ScriptSignatureFactory {


  /**
   * Creates a ScriptSignature from a given asm script
   * @param asm
   * @return
   */
  def factory(asm : Seq[ScriptToken]) : ScriptSignature = {
    val hex = asm.map(_.hex).mkString
    ScriptSignatureImpl(asm,hex)
  }

  /**
   * Creates a ScriptSignature object from a given hexadecimal script
   * @param hex
   * @return
   */
  def factory(hex : String) : ScriptSignature = {
    val asm = ScriptParser.parse(hex)
    factory(asm)
  }

  /**
   * Creates a ScriptSignature object from a given list of bytes
   * @param bytes
   * @return
   */
  def factory(bytes : List[Byte]) : ScriptSignature = {
    val hex = ScalacoinUtil.encodeHex(bytes)
    factory(hex)
  }

}
