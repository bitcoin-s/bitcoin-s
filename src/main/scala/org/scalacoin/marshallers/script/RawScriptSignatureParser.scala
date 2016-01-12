package org.scalacoin.marshallers.script

import org.scalacoin.marshallers.RawBitcoinSerializer
import org.scalacoin.protocol.script.{ScriptSignatureImpl, ScriptSignature}
import org.scalacoin.script.constant.ScriptToken
import org.scalacoin.util.ScalacoinUtil

/**
 * Created by chris on 1/12/16.
 */
trait RawScriptSignatureParser extends RawBitcoinSerializer[ScriptSignature] with ScriptParser {

  def read(hex : String) : ScriptSignature = {
    val scriptSig : List[ScriptToken] = parse(ScalacoinUtil.decodeHex(hex))
    ScriptSignatureImpl(scriptSig,hex)
  }

  def write(scriptSig : ScriptSignature) : String = scriptSig.hex

}
