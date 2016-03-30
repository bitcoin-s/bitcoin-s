package org.scalacoin.protocol.script

import org.scalacoin.marshallers.script.{RawScriptPubKeyParser, ScriptParser}
import org.scalacoin.script.bitwise.{OP_EQUAL, OP_EQUALVERIFY}
import org.scalacoin.script.constant._
import org.scalacoin.script.crypto.{OP_CHECKMULTISIG, OP_CHECKSIG, OP_HASH160}
import org.scalacoin.script.stack.OP_DUP
import org.scalacoin.util.{BitcoinScriptUtil, BitcoinSUtil, Factory, ScalacoinUtil}

/**
 * Created by chris on 1/19/16.
 */
trait ScriptPubKeyFactory extends Factory[ScriptPubKey] {

  def factory(hex : String) : ScriptPubKey = fromHex(hex)

  def factory(indicator: UpdateScriptPubKeyAsm) : ScriptPubKey = {
    fromAsm(indicator.asm)
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

  def empty : ScriptPubKey = fromAsm(List())

  def fromBytes(bytes : Seq[Byte]) : ScriptPubKey = RawScriptPubKeyParser.read(bytes)

  /**
   * Creates a scriptPubKey from its asm representation
   * @param asm
   * @return
   */
  def fromAsm(asm : Seq[ScriptToken]) : ScriptPubKey = {
    val scriptPubKeyHex = BitcoinScriptUtil.asmToHex(asm)
    asm match {
      case Seq() => EmptyScriptPubKey
      case List(OP_DUP, OP_HASH160, x : BytesToPushOntoStack, ScriptConstantImpl(pubKeyHash), OP_EQUALVERIFY, OP_CHECKSIG) =>
        P2PKHScriptPubKeyImpl(scriptPubKeyHex,asm)
      case List(OP_HASH160, x : BytesToPushOntoStack, ScriptConstantImpl(scriptHash), OP_EQUAL) =>
        P2SHScriptPubKeyImpl(scriptPubKeyHex,asm)
      case List(b : BytesToPushOntoStack, x : ScriptConstant, OP_CHECKSIG) => P2PKScriptPubKeyImpl(scriptPubKeyHex,asm)
      //TODO: make this more robust, this isn't the pattern that multsignature scriptPubKeys follow
      case _ if (asm.size > 0 && asm.contains(OP_CHECKMULTISIG) && asm.count(_.isInstanceOf[ScriptNumberOperation]) >= 2) =>
        MultiSignatureScriptPubKeyImpl(scriptPubKeyHex,asm)
      case _ => NonStandardScriptPubKeyImpl(scriptPubKeyHex,asm)
    }
  }

}

object ScriptPubKeyFactory extends ScriptPubKeyFactory
sealed trait ScriptPubKeyUpdateIndicator
case class UpdateScriptPubKeyAsm(asm : Seq[ScriptToken]) extends ScriptPubKeyUpdateIndicator
case class UpdateScriptPubKeyBytes(bytes : Seq[Byte]) extends ScriptPubKeyUpdateIndicator

