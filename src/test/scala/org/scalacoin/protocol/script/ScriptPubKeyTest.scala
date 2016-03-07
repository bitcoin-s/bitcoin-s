package org.scalacoin.protocol.script

import org.scalacoin.protocol.{MultiSignature, P2SH, P2PKH}
import org.scalacoin.script.bitwise.OP_EQUALVERIFY
import org.scalacoin.script.constant.{ScriptConstantImpl, BytesToPushOntoStackImpl, ScriptToken}
import org.scalacoin.script.crypto.{OP_CHECKSIG, OP_HASH160, OP_CODESEPARATOR}
import org.scalacoin.script.stack.OP_DUP
import org.scalacoin.util.{ScalacoinUtil, TestUtil}
import org.scalatest.{MustMatchers, FlatSpec}

/**
 * Created by chris on 1/14/16.
 */
class ScriptPubKeyTest extends FlatSpec with MustMatchers {


  val expectedAsm : Seq[ScriptToken] =
    List(OP_DUP, OP_HASH160, BytesToPushOntoStackImpl(20), ScriptConstantImpl("31a420903c05a0a7de2de40c9f02ebedbacdc172"), OP_EQUALVERIFY, OP_CHECKSIG)
  //from b30d3148927f620f5b1228ba941c211fdabdae75d0ba0b688a58accbf018f3cc
  val rawScriptPubKey = TestUtil.rawP2PKHScriptPubKey
  val scriptPubKey = ScriptPubKeyFactory.fromHex(rawScriptPubKey)
  "ScriptPubKey"  must "give the expected asm from creating a scriptPubKey from hex" in {
    scriptPubKey.asm must be (expectedAsm)
  }

  it must "derive a P2PKH script type from a scriptPubKey" in {
    scriptPubKey.scriptType must be (P2PKH)
  }

  it must "derive a P2SH script type for a scriptPubKey" in {

    TestUtil.p2shScriptPubKey.scriptType must be (P2SH)
  }



  it must "derive a multisignature script type for a scriptPubKey" in {
    val multiSigRawScriptPubKey = "5221025878e270211662a27181cf" +
      "4d6ad4d2cf0e69a98a3815c086f587c7e9388d87182103fc85980e3fac1f3d" +
      "8a5c3223c3ef5bffc1bd42d2cc42add8c3899cc66e7f1906210215b5bd0508" +
      "69166a70a7341b4f216e268b7c6c7504576dcea2cce7d11cc9a35f53ae"
    val multiSigScriptPubKey = ScriptPubKeyFactory.fromHex(multiSigRawScriptPubKey)

    multiSigScriptPubKey.scriptType must be (MultiSignature)
  }

  it must "derive the amount of required signatures from a multisignature script" in {
    val multiSigRawScriptPubKeyHex = "5221025878e270211662a27181cf" +
      "4d6ad4d2cf0e69a98a3815c086f587c7e9388d87182103fc85980e3fac1f3d" +
      "8a5c3223c3ef5bffc1bd42d2cc42add8c3899cc66e7f1906210215b5bd0508" +
      "69166a70a7341b4f216e268b7c6c7504576dcea2cce7d11cc9a35f53ae"
    val multiSigRawScriptPubKey = MultiSignatureScriptPubKeyImpl(multiSigRawScriptPubKeyHex)

    multiSigRawScriptPubKey.requiredSigs must be (2)
    multiSigRawScriptPubKey.maxSigs must be (3)
  }
}
