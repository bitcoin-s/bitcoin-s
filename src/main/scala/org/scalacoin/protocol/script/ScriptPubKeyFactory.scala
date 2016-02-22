package org.scalacoin.protocol.script

import org.scalacoin.marshallers.script.ScriptParser
import org.scalacoin.script.constant.ScriptToken

/**
 * Created by chris on 1/19/16.
 */
object ScriptPubKeyFactory {

  def factory(hex : String) : ScriptPubKey = {
    val asm : Seq[ScriptToken] = ScriptParser.parse(hex)
    ScriptPubKeyImpl(asm,hex,Seq())
  }

  def factory(asm : Seq[ScriptToken]) : ScriptPubKey = {
    val hex = asm.map(_.hex).mkString
    ScriptPubKeyImpl(asm,hex,Seq())
  }

  def factory(bytes : List[Byte]) : ScriptPubKey = {
    val asm = ScriptParser.parse(bytes.toList)
    factory(asm)
  }

  def empty = ScriptPubKeyImpl(Seq(),"",Seq())

}
