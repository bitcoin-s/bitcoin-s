package org.scalacoin.marshallers.script

import org.scalacoin.marshallers.RawBitcoinSerializer
import org.scalacoin.protocol.script.{ScriptSignatureImpl, ScriptSignature}
import org.scalacoin.script.constant.ScriptToken
import org.scalacoin.util.ScalacoinUtil

/**
 * Created by chris on 1/12/16.
 */
trait RawScriptSignatureParser extends RawBitcoinSerializer[ScriptSignature]  {

  def read(bytes : List[Byte]) : ScriptSignature = {
    val scriptSig : List[ScriptToken] = ScriptParser.parse(bytes)
    ScriptSignatureImpl(scriptSig,ScalacoinUtil.encodeHex(bytes))
  }

  def write(scriptSig : ScriptSignature) : String = scriptSig.hex
}

object RawScriptSignatureParser extends RawScriptSignatureParser
