package org.scalacoin.marshallers.script

import org.scalacoin.marshallers.RawBitcoinSerializer
import org.scalacoin.protocol.script.{ScriptPubKeyFactory, ScriptPubKeyImpl, ScriptPubKey}
import org.scalacoin.script.constant.ScriptToken
import org.scalacoin.util.{BitcoinSUtil}
import org.slf4j.LoggerFactory

/**
 * Created by chris on 1/12/16.
 */
trait RawScriptPubKeyParser extends RawBitcoinSerializer[ScriptPubKey] {


  private def logger = LoggerFactory.getLogger(this.getClass())

  override def read(bytes : List[Byte]) : ScriptPubKey = {
    require(bytes.size > 0, "Cannot parse a scriptPubKey from an empty byte list")
    //first byte indicates how many bytes the script is

    val script : List[ScriptToken] = ScriptParser.fromBytes(bytes)
    logger.info("parsed script; " + script)
    //not sure how to get addresses from a scriptPubKey
    ScriptPubKeyFactory.fromAsm(script)
  }

  override def write(scriptPubKey : ScriptPubKey) : String = {
    scriptPubKey.hex
  }
}

object RawScriptPubKeyParser extends RawScriptPubKeyParser
