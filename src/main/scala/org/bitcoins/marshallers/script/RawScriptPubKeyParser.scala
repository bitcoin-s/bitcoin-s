package org.bitcoins.marshallers.script

import org.bitcoins.marshallers.RawBitcoinSerializer
import org.bitcoins.protocol.script.{ScriptPubKey}
import org.bitcoins.script.constant.ScriptToken
import org.bitcoins.util.BitcoinSLogger
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
