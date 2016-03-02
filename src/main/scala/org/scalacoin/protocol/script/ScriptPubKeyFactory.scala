package org.scalacoin.protocol.script

import org.scalacoin.marshallers.script.{RawScriptPubKeyParser, ScriptParser}
import org.scalacoin.script.constant.ScriptToken
import org.scalacoin.util.{BitcoinSUtil, Factory, ScalacoinUtil}

/**
 * Created by chris on 1/19/16.
 */
trait ScriptPubKeyFactory extends Factory[ScriptPubKey] {

  def factory(hex : String) : ScriptPubKey = fromHex(hex)

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
  def factory(bytes : Seq[Byte]) : ScriptPubKey =  fromBytes(bytes)

  def factory(bytes : Array[Byte]) : ScriptPubKey = fromBytes(bytes.toSeq)

  def empty : ScriptPubKey = ScriptPubKeyImpl(Seq(),"",Seq())

  def fromBytes(bytes : Seq[Byte]) : ScriptPubKey = RawScriptPubKeyParser.read(bytes)

}

sealed trait ScriptPubKeyUpdateIndicator
case class UpdateScriptPubKeyAsm(asm : Seq[ScriptToken]) extends ScriptPubKeyUpdateIndicator
case class UpdateScriptPubKeyBytes(bytes : Seq[Byte]) extends ScriptPubKeyUpdateIndicator
object ScriptPubKeyFactory extends ScriptPubKeyFactory
