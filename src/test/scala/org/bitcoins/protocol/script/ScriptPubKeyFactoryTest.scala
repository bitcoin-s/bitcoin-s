package org.bitcoins.protocol.script

import org.bitcoins.util.{TestUtil, BitcoinjConversions, BitcoinJTestUtil, BitcoinSUtil}
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 3/2/16.
 */
class ScriptPubKeyFactoryTest extends FlatSpec with MustMatchers {

  "ScriptPubKeyFactory" must "create a scriptPubKey from a sequences of bytes and hex and get the same thing" in {
    //from b30d3148927f620f5b1228ba941c211fdabdae75d0ba0b688a58accbf018f3cc
    val rawScriptPubKey = TestUtil.rawP2PKHScriptPubKey
    val scriptPubKeyFromBytes = ScriptPubKey(BitcoinSUtil.decodeHex(rawScriptPubKey))
    val scriptPubKeyFromHex = ScriptPubKey(rawScriptPubKey)

    scriptPubKeyFromBytes must be (scriptPubKeyFromHex)
  }

  it must "create a multisignature scriptPubKey from a sequence of bytes and get the same thing back" in {
    val (_,_,bitcoinJScriptPubKey) = BitcoinJTestUtil.signedMultiSigTransaction
    ScriptPubKey(bitcoinJScriptPubKey.getProgram).bytes must be
    (BitcoinSUtil.encodeHex(bitcoinJScriptPubKey.getProgram))
  }

  it must "create a multsignature scriptPubkey from its hexadecimal representation and then convert it back to the original hex" in {
    val (_,_,bitcoinJScriptPubKey) = BitcoinJTestUtil.signedMultiSigTransaction
    BitcoinjConversions.toScriptPubKey(bitcoinJScriptPubKey).hex must be
    (BitcoinSUtil.encodeHex(bitcoinJScriptPubKey.getProgram))
  }

  it must "create a scriptPubKey from an empty string" in {
    val scriptPubKey = ScriptPubKey("")
    scriptPubKey.hex must be ("")
  }

  it must "create a p2pk scriptPubKey from its hexadecimal representation" in {
    val rawScriptPubKey = TestUtil.rawP2PKScriptPubKey
    val scriptPubKey = ScriptPubKey(rawScriptPubKey)
    val result = scriptPubKey match {
      case script : P2PKScriptPubKey => true
      case _ => false
    }
    result must be (true)

  }

  it must "create a multisignature scriptPubKey from a script using OP_CHECKMULTISIGVERIFY" in {
    val multiSigRawScriptPubKeyHex = "5221025878e270211662a27181cf" +
      "4d6ad4d2cf0e69a98a3815c086f587c7e9388d87182103fc85980e3fac1f3d" +
      "8a5c3223c3ef5bffc1bd42d2cc42add8c3899cc66e7f1906210215b5bd0508" +
      "69166a70a7341b4f216e268b7c6c7504576dcea2cce7d11cc9a35f53af"
    val scriptPubKey = ScriptPubKey(multiSigRawScriptPubKeyHex)
    val isMultiSigScriptPubKey : Boolean = scriptPubKey match {
      case s : MultiSignatureScriptPubKey => true
      case _ => false
    }

    isMultiSigScriptPubKey must be (true)

  }

}
