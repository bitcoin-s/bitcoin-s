package org.bitcoins.core.serializers.script

import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.script.constant.{ScriptConstant, ScriptToken}
import org.bitcoins.core.script.crypto.{OP_CHECKMULTISIG, OP_CHECKMULTISIGVERIFY}
import org.bitcoins.core.util.{BitcoinSLogger, BitcoinSUtil}
import org.slf4j.LoggerFactory

/**
 * Created by chris on 1/12/16.
 */
trait RawScriptSignatureParser extends RawBitcoinSerializer[ScriptSignature] with BitcoinSLogger  {

  def read(bytes : List[Byte]) : ScriptSignature = {
    if (bytes.isEmpty) EmptyScriptSignature
    else {
      val compactSizeUInt = CompactSizeUInt.parseCompactSizeUInt(bytes)
      val scriptSigBytes = bytes.slice(compactSizeUInt.size.toInt, compactSizeUInt.num.toInt + compactSizeUInt.size.toInt)
      val scriptTokens : List[ScriptToken] = ScriptParser.fromBytes(scriptSigBytes)
      ScriptSignature.fromAsm(scriptTokens)
    }
  }

  def write(scriptSig : ScriptSignature) : String = scriptSig.hex
}

object RawScriptSignatureParser extends RawScriptSignatureParser
