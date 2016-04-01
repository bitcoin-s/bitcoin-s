package org.scalacoin.protocol.script

import org.scalacoin.marshallers.script.{RawScriptPubKeyParser, ScriptParser}
import org.scalacoin.script.bitwise.{OP_EQUAL, OP_EQUALVERIFY}
import org.scalacoin.script.constant._
import org.scalacoin.script.crypto.{OP_CHECKMULTISIGVERIFY, OP_CHECKMULTISIG, OP_CHECKSIG, OP_HASH160}
import org.scalacoin.script.stack.OP_DUP
import org.scalacoin.util.{BitcoinScriptUtil, BitcoinSUtil, Factory}

/**
 * Created by chris on 1/19/16.
 */
trait ScriptPubKeyFactory extends Factory[ScriptPubKey] {

  def factory(hex : String) : ScriptPubKey = fromHex(hex)

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
      case _ if (isMultiSignatureScriptPubKey(asm)) =>
        MultiSignatureScriptPubKeyImpl(scriptPubKeyHex,asm)
      case _ => NonStandardScriptPubKeyImpl(scriptPubKeyHex,asm)
    }
  }


  /**
   * Determines if the given script tokens are a multisignature scriptPubKey
   * @param asm the tokens to check
   * @return a boolean indicating if the given tokens are a multisignature scriptPubKey
   */
  private def isMultiSignatureScriptPubKey(asm : Seq[ScriptToken]) : Boolean = {
    val isNotEmpty = asm.size > 0
    val containsMultSigOp = asm.contains(OP_CHECKMULTISIG) || asm.contains(OP_CHECKMULTISIGVERIFY)
    //we need at least two script operations to indicate m required signatures & n maximum signatures
    val has2ScriptOperations = asm.count(_.isInstanceOf[ScriptNumberOperation]) >= 2
    isNotEmpty && containsMultSigOp && has2ScriptOperations

  }

}

object ScriptPubKeyFactory extends ScriptPubKeyFactory
sealed trait ScriptPubKeyUpdateIndicator
case class UpdateScriptPubKeyAsm(asm : Seq[ScriptToken]) extends ScriptPubKeyUpdateIndicator
case class UpdateScriptPubKeyBytes(bytes : Seq[Byte]) extends ScriptPubKeyUpdateIndicator

