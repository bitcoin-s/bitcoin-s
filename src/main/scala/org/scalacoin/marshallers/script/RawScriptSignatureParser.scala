package org.scalacoin.marshallers.script

import org.scalacoin.marshallers.RawBitcoinSerializer
import org.scalacoin.protocol.script.{ScriptSignatureImpl, ScriptSignature}
import org.scalacoin.script.constant.ScriptToken
import org.scalacoin.util.ScalacoinUtil

/**
 * Created by chris on 1/12/16.
 */
trait RawScriptSignatureParser extends RawBitcoinSerializer[ScriptSignature] with ScriptParser {

  def read(bytes : List[Byte]) : ScriptSignature = {
    require(bytes.size > 0, this.getClass().toString + " cannot parse an empty byte list")
    //first byte indicates how large the ENTIRE script signature is
    //see https://bitcoin.org/en/developer-reference#txout for example
    val scriptSigSize = bytes.head.toInt
    val scriptSig : List[ScriptToken] = parse(bytes.tail)
    ScriptSignatureImpl(scriptSig,ScalacoinUtil.encodeHex(bytes))
  }

  def write(scriptSig : ScriptSignature) : String = scriptSig.hex
}

object RawScriptSignatureParser extends RawScriptSignatureParser
