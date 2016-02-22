package org.scalacoin.protocol.script

import org.scalacoin.marshallers.script.ScriptParser
import org.scalacoin.script.constant.ScriptToken

/**
 * Created by chris on 1/19/16.
 */
trait ScriptPubKeyFactory {

  def factory(hex : String) : ScriptPubKey = {
    val asm : Seq[ScriptToken] = ScriptParser.parse(hex)
    ScriptPubKeyImpl(asm,hex,Seq())
  }

  def factory(indicator: UpdateScriptPubKeyAsm) : ScriptPubKey = {
    val hex = indicator.asm.map(_.hex).mkString
    ScriptPubKeyImpl(indicator.asm,hex,Seq())
  }

  def factory(indicator: UpdateScriptPubKeyBytes) : ScriptPubKey = {
    val asm = ScriptParser.parse(indicator.bytes)
    factory(UpdateScriptPubKeyAsm(asm))
  }

  def empty = ScriptPubKeyImpl(Seq(),"",Seq())

}

sealed trait ScriptPubKeyUpdateIndicator
case class UpdateScriptPubKeyAsm(asm : Seq[ScriptToken]) extends ScriptPubKeyUpdateIndicator
case class UpdateScriptPubKeyBytes(bytes : Seq[Byte]) extends ScriptPubKeyUpdateIndicator
object ScriptPubKeyFactory extends ScriptPubKeyFactory
