package org.bitcoins.core.serializers.script

import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.script.constant.ScriptToken
import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.core.util.BitcoinSLogger

import scala.util.Try

/**
 * Created by chris on 1/12/16.
 */
sealed abstract class RawScriptSignatureParser extends RawBitcoinSerializer[ScriptSignature] {

  def read(bytes: List[Byte]): ScriptSignature = {
    if (bytes.isEmpty) EmptyScriptSignature
    else {
      val compactSizeUInt = CompactSizeUInt.parseCompactSizeUInt(bytes)
      //TODO: Figure out a better way to do this, we can theoretically have numbers larger than Int.MaxValue,
      val scriptSigBytes = bytes.slice(
        compactSizeUInt.size.toInt,
        compactSizeUInt.num.toInt + compactSizeUInt.size.toInt
      )
      val scriptTokens: List[ScriptToken] = ScriptParser.fromBytes(scriptSigBytes)
      ScriptSignature.fromAsm(scriptTokens)
    }
  }

  def write(scriptSig: ScriptSignature): Seq[Byte] = scriptSig.bytes
}

object RawScriptSignatureParser extends RawScriptSignatureParser
