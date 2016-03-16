package org.scalacoin.protocol.script

import org.scalacoin.util.{TestUtil, BitcoinjConversions, BitcoinJTestUtil, BitcoinSUtil}
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 3/2/16.
 */
class ScriptPubKeyFactoryTest extends FlatSpec with MustMatchers {

  "ScriptPubKeyFactory" must "create a scriptPubKey from a sequences of bytes and hex and get the same thing" in {
    //from b30d3148927f620f5b1228ba941c211fdabdae75d0ba0b688a58accbf018f3cc
    val rawScriptPubKey = TestUtil.rawP2PKHScriptPubKey
    val scriptPubKeyFromBytes = ScriptPubKeyFactory.fromBytes(BitcoinSUtil.decodeHex(rawScriptPubKey))
    val scriptPubKeyFromHex = ScriptPubKeyFactory.fromHex(rawScriptPubKey)

    scriptPubKeyFromBytes must be (scriptPubKeyFromHex)
  }

  it must "create a multisignature scriptPubKey from a sequence of bytes and get the same thing back" in {
    val (_,_,bitcoinJScriptPubKey) = BitcoinJTestUtil.signedMultiSigTransaction
    ScriptPubKeyFactory.fromBytes(bitcoinJScriptPubKey.getProgram).bytes must be
    (BitcoinSUtil.encodeHex(bitcoinJScriptPubKey.getProgram))
  }

  it must "create a multsignature scriptPubkey from its hexadecimal representation and then convert it back to the original hex" in {
    val (_,_,bitcoinJScriptPubKey) = BitcoinJTestUtil.signedMultiSigTransaction
    BitcoinjConversions.toScriptPubKey(bitcoinJScriptPubKey).hex must be
    (BitcoinSUtil.encodeHex(bitcoinJScriptPubKey.getProgram))
  }

  it must "create a scriptPubKey from an empty string" in {
    val scriptPubKey = ScriptPubKeyFactory.fromHex("")
    scriptPubKey.hex must be ("")
  }

  it must "create a p2pk scriptPubKey from its hexadecimal representation" in {
    val rawScriptPubKey = TestUtil.rawP2PKScriptPubKey
    val scriptPubKey = ScriptPubKeyFactory.fromHex(rawScriptPubKey)
    val result = scriptPubKey match {
      case script : P2PKScriptPubKey => true
      case _ => false
    }
    result must be (true)

  }


}
