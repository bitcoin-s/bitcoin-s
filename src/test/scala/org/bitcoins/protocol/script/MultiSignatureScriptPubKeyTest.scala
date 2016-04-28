package org.bitcoins.protocol.script

import org.bitcoins.crypto.ECFactory
import org.scalatest.{MustMatchers, FlatSpec}

/**
 * Created by chris on 3/8/16.
 */
class MultiSignatureScriptPubKeyTest extends FlatSpec with MustMatchers {

  "MultiSignatureScriptPubKey" must "derive the amount of required signatures from a multisignature script" in {
    val multiSigRawScriptPubKeyHex = "5221025878e270211662a27181cf" +
      "4d6ad4d2cf0e69a98a3815c086f587c7e9388d87182103fc85980e3fac1f3d" +
      "8a5c3223c3ef5bffc1bd42d2cc42add8c3899cc66e7f1906210215b5bd0508" +
      "69166a70a7341b4f216e268b7c6c7504576dcea2cce7d11cc9a35f53ae"
    val scriptPubKey = ScriptPubKey(multiSigRawScriptPubKeyHex)
    val multiSigScriptPubKey : MultiSignatureScriptPubKey = scriptPubKey match {
      case s : MultiSignatureScriptPubKey => s
      case _ => throw new RuntimeException("Should be a multisig script pub key")
    }


    multiSigScriptPubKey.requiredSigs must be (2)
    multiSigScriptPubKey.maxSigs must be (3)
  }

  it must "derive the public keys encoded inside of a multisignature script" in {
    val multiSigRawScriptPubKeyHex = "5221025878e270211662a27181cf" +
      "4d6ad4d2cf0e69a98a3815c086f587c7e9388d87182103fc85980e3fac1f3d" +
      "8a5c3223c3ef5bffc1bd42d2cc42add8c3899cc66e7f1906210215b5bd0508" +
      "69166a70a7341b4f216e268b7c6c7504576dcea2cce7d11cc9a35f53ae"
    val scriptPubKey = ScriptPubKey(multiSigRawScriptPubKeyHex)
    val multiSigScriptPubKey : MultiSignatureScriptPubKey = scriptPubKey match {
      case s : MultiSignatureScriptPubKey => s
      case _ => throw new RuntimeException("Should be a multisig script pub key")
    }

    multiSigScriptPubKey.publicKeys must be (Seq(
      ECFactory.publicKey("025878e270211662a27181cf4d6ad4d2cf0e69a98a3815c086f587c7e9388d8718"),
      ECFactory.publicKey("03fc85980e3fac1f3d8a5c3223c3ef5bffc1bd42d2cc42add8c3899cc66e7f1906"),
      ECFactory.publicKey("0215b5bd050869166a70a7341b4f216e268b7c6c7504576dcea2cce7d11cc9a35f")
    ))

  }


  it must "find the public keys without the public inside of the multisignature script with an OP_NOT at the end" in {
    val rawPubKey = "522102865c40293a680cb9c020e7b1e106d8c1916d3cef99aa431a56d253e69256dac02102865c40293a680cb9c020e7b1e106d8c1916d3cef99aa431a56d253e69256dac052ae91"
    val scriptPubKey = ScriptPubKey(rawPubKey)
    val multiSigScriptPubKey : MultiSignatureScriptPubKey = scriptPubKey match {
      case s : MultiSignatureScriptPubKey => s
      case _ => throw new RuntimeException("Should be a multisig script pub key")
    }

    multiSigScriptPubKey.publicKeys must be (Seq(
      ECFactory.publicKey("02865c40293a680cb9c020e7b1e106d8c1916d3cef99aa431a56d253e69256dac0"),
      ECFactory.publicKey("02865c40293a680cb9c020e7b1e106d8c1916d3cef99aa431a56d253e69256dac0")
    ))

  }


  it must "find the required signatures from a multisignature scriptPubKey using an OP_CHECKMULTISIGVERFIY" in {
    val multiSigRawScriptPubKeyHex = "5221025878e270211662a27181cf" +
      "4d6ad4d2cf0e69a98a3815c086f587c7e9388d87182103fc85980e3fac1f3d" +
      "8a5c3223c3ef5bffc1bd42d2cc42add8c3899cc66e7f1906210215b5bd0508" +
      "69166a70a7341b4f216e268b7c6c7504576dcea2cce7d11cc9a35f53af"
    val scriptPubKey = ScriptPubKey(multiSigRawScriptPubKeyHex)
    val multiSigScriptPubKey : MultiSignatureScriptPubKey = scriptPubKey match {
      case s : MultiSignatureScriptPubKey => s
      case _ => throw new RuntimeException("Should be a multisig script pub key")
    }

    multiSigScriptPubKey.maxSigs must be (3)
    multiSigScriptPubKey.requiredSigs must be (2)

  }


}
