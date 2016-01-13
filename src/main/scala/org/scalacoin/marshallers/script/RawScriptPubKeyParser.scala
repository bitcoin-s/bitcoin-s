package org.scalacoin.marshallers.script

import org.scalacoin.marshallers.RawBitcoinSerializer
import org.scalacoin.protocol.script.{ScriptPubKeyImpl, ScriptPubKey}
import org.scalacoin.script.constant.ScriptToken
import org.scalacoin.util.ScalacoinUtil

/**
 * Created by chris on 1/12/16.
 */
trait RawScriptPubKeyParser extends RawBitcoinSerializer[ScriptPubKey] with ScriptParser {


  override def read(bytes : List[Byte]) : ScriptPubKey = {
    require(bytes.size > 0, "Cannot parse a scriptPubKey from an empty byte list")
    //first byte indicates how many bytes the script is
    val scriptSize = bytes.head
    val script : List[ScriptToken] = parse(bytes.tail)
    //not sure how to get addresses from a scriptPubKey
    ScriptPubKeyImpl(script,ScalacoinUtil.encodeHex(bytes),Seq())
  }

  override def write(scriptPubKey : ScriptPubKey) : String = {
    scriptPubKey.hex
  }
}

object RawScriptPubKeyParser extends RawScriptPubKeyParser
