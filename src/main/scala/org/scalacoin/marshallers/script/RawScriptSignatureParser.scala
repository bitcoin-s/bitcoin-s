package org.scalacoin.marshallers.script

import org.scalacoin.marshallers.RawBitcoinSerializer
import org.scalacoin.protocol.script.{ScriptSignatureFactory, ScriptSignatureImpl, ScriptSignature}
import org.scalacoin.script.constant.ScriptToken
import org.scalacoin.script.crypto.{OP_CHECKMULTISIGVERIFY, OP_CHECKMULTISIG}
import org.scalacoin.util.{BitcoinSUtil}

/**
 * Created by chris on 1/12/16.
 */
trait RawScriptSignatureParser extends RawBitcoinSerializer[ScriptSignature]  {

  def read(bytes : List[Byte]) : ScriptSignature = {
    val scriptSig : List[ScriptToken] = ScriptParser.parse(bytes)
    //check to see if the last script token can be parsed into a redeemScript
    //for a p2sh input script
    if (scriptSig.size > 0 && isRedeemScript(scriptSig.last)) {
      val redeemScript : List[ScriptToken] = ScriptParser.parse(scriptSig.last.bytes)
      //remove the last element and then append the redeemScript
      val scriptSigWithRedeemScript = scriptSig.reverse.tail.reverse ++ redeemScript
      ScriptSignatureImpl(scriptSigWithRedeemScript,BitcoinSUtil.encodeHex(bytes))
    } else ScriptSignatureImpl(scriptSig,BitcoinSUtil.encodeHex(bytes))
  }

  def write(scriptSig : ScriptSignature) : String = scriptSig.hex


  /**
   * Detects if the given script token is a redeem script
   * @param token
   * @return
   */
  private def isRedeemScript(token : ScriptToken) : Boolean = {
    val redeemScript = ScriptParser.parse(token.bytes)
    redeemScript.contains(OP_CHECKMULTISIG) || redeemScript.contains(OP_CHECKMULTISIGVERIFY)
  }
}

object RawScriptSignatureParser extends RawScriptSignatureParser
