package org.scalacoin.protocol.script

import org.scalacoin.marshallers.script.ScriptParser
import org.scalacoin.script.constant.ScriptToken
import org.scalacoin.util.ScalacoinUtil

/**
 * Created by chris on 1/19/16.
 */
trait ScriptPubKeyFactory {

  def factory(hex : String) : ScriptPubKey = factory(ScalacoinUtil.decodeHex(hex))

  def factory(indicator: UpdateScriptPubKeyAsm) : ScriptPubKey = {
    val hex = indicator.asm.map(_.hex).mkString
    ScriptPubKeyImpl(indicator.asm,hex,Seq())
  }

  def factory(indicator: UpdateScriptPubKeyBytes) : ScriptPubKey = {
    val asm = ScriptParser.parse(indicator.bytes)
    factory(UpdateScriptPubKeyAsm(asm))
  }

  /**
   * Parses a script from its byte represenatation and returns a ScriptPubKey
   * @param bytes
   * @return
   */
  def factory(bytes : Seq[Byte]) : ScriptPubKey =  {
    val asm = ScriptParser.parse(bytes)
    ScriptPubKeyImpl(asm,ScalacoinUtil.encodeHex(bytes),Seq())
  }

  def factory(bytes : Array[Byte]) : ScriptPubKey= factory(bytes.toSeq)

  def empty : ScriptPubKey= ScriptPubKeyImpl(Seq(),"",Seq())

}

sealed trait ScriptPubKeyUpdateIndicator
case class UpdateScriptPubKeyAsm(asm : Seq[ScriptToken]) extends ScriptPubKeyUpdateIndicator
case class UpdateScriptPubKeyBytes(bytes : Seq[Byte]) extends ScriptPubKeyUpdateIndicator
object ScriptPubKeyFactory extends ScriptPubKeyFactory
