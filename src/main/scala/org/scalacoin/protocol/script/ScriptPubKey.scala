package org.scalacoin.protocol.script

import org.scalacoin.marshallers.script.{ScriptParser, RawScriptPubKeyParser}
import org.scalacoin.marshallers.transaction.TransactionElement
import org.scalacoin.protocol._
import org.scalacoin.script.bitwise.{OP_EQUAL, OP_EQUALVERIFY}
import org.scalacoin.script.constant._
import org.scalacoin.script.crypto.{OP_CHECKMULTISIG, OP_CHECKSIG, OP_HASH160}
import org.scalacoin.script.stack.OP_DUP
import org.scalacoin.util.BitcoinSUtil

/**
 * Created by chris on 12/26/15.
 */
sealed trait ScriptPubKey extends TransactionElement {

  /**
   * Representation of a scriptSignature in a parsed assembly format
   * this data structure can be run through the script interpreter to
   * see if a script evaluates to true
   * @return
   */
  def asm : Seq[ScriptToken] = ScriptParser.fromBytes(bytes)


  def reqSigs : Option[Int] = {
    scriptType match {
      case P2PKH => Some(1)
      //TODO: Figure out how many signatures are actually required by the scriptPubKey
      case P2SH => None
      case MultiSignature =>
        val signatureCount = asm.count(_.isInstanceOf[ScriptConstant])
        Some(signatureCount)
      case NonStandard => None
    }
  }
  def scriptType : ScriptType = {
    asm match {
      case List(OP_DUP, OP_HASH160, BytesToPushOntoStackImpl(x), ScriptConstantImpl(pubKeyHash), OP_EQUALVERIFY, OP_CHECKSIG) => P2PKH
      case List(OP_HASH160, BytesToPushOntoStackImpl(x), ScriptConstantImpl(scriptHash), OP_EQUAL) => P2SH
      //TODO: make this more robust, this isn't the pattern that multsignature scriptPubKeys follow
      case _ if (asm.last == OP_CHECKMULTISIG) => MultiSignature
      case _ => NonStandard
    }
  }

  //the addresses that the bitcoins correlated to the output
  def addresses : Seq[BitcoinAddress] = ???

}


sealed case class ScriptPubKeyImpl(hex : String) extends ScriptPubKey
sealed case class P2PKHScriptPubKey(hex : String) extends ScriptPubKey
sealed case class MultiSignatureScriptPubKey(hex : String) extends ScriptPubKey
sealed case class P2SHScriptPubKey(hex : String) extends ScriptPubKey