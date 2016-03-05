package org.scalacoin.protocol.script

import org.scalacoin.marshallers.script.{RawScriptPubKeyParser, ScriptParser}
import org.scalacoin.protocol.{NonStandard, MultiSignature, P2SH, P2PKH}
import org.scalacoin.script.bitwise.{OP_EQUAL, OP_EQUALVERIFY}
import org.scalacoin.script.constant.{ScriptConstantImpl, BytesToPushOntoStackImpl, ScriptToken}
import org.scalacoin.script.crypto.{OP_CHECKMULTISIG, OP_CHECKSIG, OP_HASH160}
import org.scalacoin.script.stack.OP_DUP
import org.scalacoin.util.{BitcoinScriptUtil, BitcoinSUtil, Factory, ScalacoinUtil}

/**
 * Created by chris on 1/19/16.
 */
trait ScriptPubKeyFactory extends Factory[ScriptPubKey] {

  def factory(hex : String) : ScriptPubKey = fromHex(hex)

  def factory(indicator: UpdateScriptPubKeyAsm) : ScriptPubKey = {
    val hex = indicator.asm.map(_.hex).mkString
    ScriptPubKeyImpl(hex)
  }

  def factory(indicator: UpdateScriptPubKeyBytes) : ScriptPubKey = {
    val asm = ScriptParser.fromBytes(indicator.bytes)
    factory(UpdateScriptPubKeyAsm(asm))
  }

  /**
   * Parses a script from its byte represenatation and returns a ScriptPubKey
   * @param bytes
   * @return
   */
  def factory(bytes : Seq[Byte]) : ScriptPubKey =  fromBytes(bytes)

  def factory(bytes : Array[Byte]) : ScriptPubKey = fromBytes(bytes.toSeq)

  def empty : ScriptPubKey = ScriptPubKeyImpl("")

  def fromBytes(bytes : Seq[Byte]) : ScriptPubKey = RawScriptPubKeyParser.read(bytes)

  def fromAsm(asm : Seq[ScriptToken]) : ScriptPubKey = {
    val scriptPubKeyHex = BitcoinScriptUtil.asmToHex(asm)
    asm match {
      case List(OP_DUP, OP_HASH160, BytesToPushOntoStackImpl(x), ScriptConstantImpl(pubKeyHash), OP_EQUALVERIFY, OP_CHECKSIG) => P2PKHScriptPubKey(scriptPubKeyHex)
      case List(OP_HASH160, BytesToPushOntoStackImpl(x), ScriptConstantImpl(scriptHash), OP_EQUAL) => P2SHScriptPubKey(scriptPubKeyHex)
      //TODO: make this more robust, this isn't the pattern that multisignature scriptPubKeys follow
      case _ if (asm.last == OP_CHECKMULTISIG) => MultiSignatureScriptPubKey(scriptPubKeyHex)
      case _ => ScriptPubKeyImpl(scriptPubKeyHex)
    }
  }

}

sealed trait ScriptPubKeyUpdateIndicator
case class UpdateScriptPubKeyAsm(asm : Seq[ScriptToken]) extends ScriptPubKeyUpdateIndicator
case class UpdateScriptPubKeyBytes(bytes : Seq[Byte]) extends ScriptPubKeyUpdateIndicator
object ScriptPubKeyFactory extends ScriptPubKeyFactory
