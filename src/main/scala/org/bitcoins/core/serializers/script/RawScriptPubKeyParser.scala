package org.bitcoins.core.serializers.script

import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.core.protocol.script.{EmptyScriptPubKey, ScriptPubKey}
import org.bitcoins.core.script.constant.ScriptToken
import org.bitcoins.core.util.{BitcoinSLogger, BitcoinSUtil}
import org.slf4j.LoggerFactory

/**
 * Created by chris on 1/12/16.
 */
trait RawScriptPubKeyParser extends RawBitcoinSerializer[ScriptPubKey] {

  override def read(bytes : List[Byte]) : ScriptPubKey = {
    if (bytes.isEmpty) EmptyScriptPubKey
    else {
      val compactSizeUInt = CompactSizeUInt.parseCompactSizeUInt(bytes)
      val scriptPubKeyBytes = bytes.slice(compactSizeUInt.size.toInt, compactSizeUInt.num.toInt + compactSizeUInt.size.toInt)
      val script : List[ScriptToken] = ScriptParser.fromBytes(scriptPubKeyBytes)
      ScriptPubKey.fromAsm(script)
    }
  }

  override def write(scriptPubKey : ScriptPubKey) : String = scriptPubKey.hex
}

object RawScriptPubKeyParser extends RawScriptPubKeyParser
