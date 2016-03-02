package org.scalacoin.protocol.script

import org.scalacoin.util.BitcoinSUtil
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 3/2/16.
 */
class ScriptPubKeyFactoryTest extends FlatSpec with MustMatchers {

  "ScriptPubKeyFactory" must "create a scriptPubKey from a sequences of bytes and hex and get the same thing" in {
    //from b30d3148927f620f5b1228ba941c211fdabdae75d0ba0b688a58accbf018f3cc
    val rawScriptPubKey = "76a91431a420903c05a0a7de2de40c9f02ebedbacdc17288ac"
    val scriptPubKeyFromBytes = ScriptPubKeyFactory.fromBytes(BitcoinSUtil.decodeHex(rawScriptPubKey))
    val scriptPubKeyFromHex = ScriptPubKeyFactory.fromHex(rawScriptPubKey)

    scriptPubKeyFromBytes must be (scriptPubKeyFromHex)
  }

}
