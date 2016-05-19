package org.bitcoins.core.serializers.script

import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.core.protocol.script.{ScriptPubKey}
import org.bitcoins.core.script.constant.ScriptToken
import org.bitcoins.core.util.BitcoinSLogger
import org.slf4j.LoggerFactory

/**
 * Created by chris on 1/12/16.
 */
trait RawScriptPubKeyParser extends RawBitcoinSerializer[ScriptPubKey] with BitcoinSLogger {

  override def read(bytes : List[Byte]) : ScriptPubKey = {
    val script : List[ScriptToken] = ScriptParser.fromBytes(bytes)
    ScriptPubKey.fromAsm(script)
  }

  override def write(scriptPubKey : ScriptPubKey) : String = {
    scriptPubKey.hex
  }
}

object RawScriptPubKeyParser extends RawScriptPubKeyParser
