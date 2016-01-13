package org.scalacoin.marshallers.script

import org.scalacoin.marshallers.RawBitcoinSerializer
import org.scalacoin.protocol.script.{ScriptPubKeyImpl, ScriptPubKey}
import org.scalacoin.script.constant.ScriptToken
import org.scalacoin.util.ScalacoinUtil

/**
 * Created by chris on 1/12/16.
 */
trait RawScriptPubKeyParser extends RawBitcoinSerializer[ScriptPubKey] with ScriptParser {


  override def read(hex : String) : ScriptPubKey = {
    require(hex.size > 0, "Cannot parse a scriptPubKey from an empty string")
    val bytes = ScalacoinUtil.decodeHex(hex)
    //first byte indicates how many bytes the script is
    val scriptSize = bytes.head

    val script : List[ScriptToken] = parse(bytes.tail)

    //not sure how to get addresses from a scriptPubKey
    ScriptPubKeyImpl(script,hex,Seq())
  }

  override def write(scriptPubKey : ScriptPubKey) : String = ???
}
