package org.scalacoin.marshallers.script

import org.scalacoin.marshallers.RawBitcoinSerializer
import org.scalacoin.protocol.script.{ScriptPubKeyFactory, ScriptPubKey}
import org.scalacoin.script.constant.ScriptToken
import org.slf4j.LoggerFactory

/**
 * Created by chris on 1/12/16.
 */
trait RawScriptPubKeyParser extends RawBitcoinSerializer[ScriptPubKey] {


  private def logger = LoggerFactory.getLogger(this.getClass())

  override def read(bytes : List[Byte]) : ScriptPubKey = {
    val script : List[ScriptToken] = ScriptParser.fromBytes(bytes)
    ScriptPubKeyFactory.fromAsm(script)
  }

  override def write(scriptPubKey : ScriptPubKey) : String = {
    scriptPubKey.hex
  }
}

object RawScriptPubKeyParser extends RawScriptPubKeyParser
