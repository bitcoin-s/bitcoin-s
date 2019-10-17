package org.bitcoins.core.protocol.script

import org.bitcoins.core.crypto.ECPublicKey
import org.bitcoins.core.util.TestUtil
import org.bitcoins.testkit.util.BitcoinSUnitTest

/**
  * Created by chris on 3/8/16.
  */
class MultiSignatureScriptPubKeyTest extends BitcoinSUnitTest {

  "MultiSignatureScriptPubKey" must "derive the amount of required signatures from a multisignature script" in {
    val multiSigRawScriptPubKeyHex = TestUtil.multiSigScriptPubKeyHex

    val scriptPubKey = ScriptPubKey(multiSigRawScriptPubKeyHex)
    val multiSigScriptPubKey: MultiSignatureScriptPubKey = scriptPubKey match {
      case s: MultiSignatureScriptPubKey => s
      case _ =>
        throw new RuntimeException("Should be a multisig script pub key")
    }

    multiSigScriptPubKey.requiredSigs must be(2)
    multiSigScriptPubKey.maxSigs must be(3)
  }

  it must "derive the public keys encoded inside of a multisignature script" in {
    val multiSigRawScriptPubKeyHex = "695221025878e270211662a27181cf" +
      "4d6ad4d2cf0e69a98a3815c086f587c7e9388d87182103fc85980e3fac1f3d" +
      "8a5c3223c3ef5bffc1bd42d2cc42add8c3899cc66e7f1906210215b5bd0508" +
      "69166a70a7341b4f216e268b7c6c7504576dcea2cce7d11cc9a35f53ae"
    val scriptPubKey = ScriptPubKey(multiSigRawScriptPubKeyHex)
    val multiSigScriptPubKey: MultiSignatureScriptPubKey = scriptPubKey match {
      case s: MultiSignatureScriptPubKey => s
      case _ =>
        throw new RuntimeException("Should be a multisig script pub key")
    }

    multiSigScriptPubKey.publicKeys must be(
      Seq(
        ECPublicKey(
          "025878e270211662a27181cf4d6ad4d2cf0e69a98a3815c086f587c7e9388d8718"),
        ECPublicKey(
          "03fc85980e3fac1f3d8a5c3223c3ef5bffc1bd42d2cc42add8c3899cc66e7f1906"),
        ECPublicKey(
          "0215b5bd050869166a70a7341b4f216e268b7c6c7504576dcea2cce7d11cc9a35f")
      ))

  }

  it must "find the required signatures from a multisignature scriptPubKey using an OP_CHECKMULTISIGVERFIY" in {
    val multiSigRawScriptPubKeyHex = "695221025878e270211662a27181cf" +
      "4d6ad4d2cf0e69a98a3815c086f587c7e9388d87182103fc85980e3fac1f3d" +
      "8a5c3223c3ef5bffc1bd42d2cc42add8c3899cc66e7f1906210215b5bd0508" +
      "69166a70a7341b4f216e268b7c6c7504576dcea2cce7d11cc9a35f53af"
    val scriptPubKey = ScriptPubKey(multiSigRawScriptPubKeyHex)
    val multiSigScriptPubKey: MultiSignatureScriptPubKey = scriptPubKey match {
      case s: MultiSignatureScriptPubKey => s
      case _ =>
        throw new RuntimeException("Should be a multisig script pub key")
    }

    multiSigScriptPubKey.maxSigs must be(3)
    multiSigScriptPubKey.requiredSigs must be(2)

  }

}
