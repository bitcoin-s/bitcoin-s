package org.bitcoins.core.protocol.script

import org.bitcoins.core.crypto._
import org.bitcoins.core.script.constant.{ ScriptConstant }
import org.bitcoins.core.util.{ TestUtil, BitcoinSUtil }
import org.scalatest.{ FlatSpec, MustMatchers }

/**
 * Created by chris on 2/17/16.
 */
class ScriptSignatureFactoryTest extends FlatSpec with MustMatchers {

  "ScriptSignatureFactory" must "give the exact same result whether parsing bytes or parsing hex" in {
    val signatureHex = "30450221008949f0cb400094ad2b5eb399d59d01c14d73d8fe6e96df1a7150deb388ab8935022079656090d7" +
      "f6bac4c9a94e0aad311a4268e082a725f8aeae0573fb12ff866a5f01"
    val signatureBytes: scodec.bits.ByteVector = BitcoinSUtil.decodeHex(signatureHex)

    val scriptSigFromHex = ScriptSignature(signatureHex)
    val scriptSigFromBytes = ScriptSignature(signatureBytes)

    scriptSigFromHex must be(scriptSigFromBytes)

  }

  it must "build a script signature from a digital signature and a public key" in {
    val digitalSignatureBytes = TestUtil.p2pkhInputScriptAsm(1).bytes
    val digitalSignature: ECDigitalSignature = ECDigitalSignature(digitalSignatureBytes)
    val publicKeyBytes = TestUtil.p2pkhInputScriptAsm(3).bytes
    val publicKey: ECPublicKey = ECPublicKey(publicKeyBytes)
    val actualScriptSig: ScriptSignature = P2PKHScriptSignature(digitalSignature, publicKey)
    actualScriptSig.asm must be(TestUtil.p2pkhInputScriptAsm)
  }

  it must "parse a p2pk scriptSignature from a raw scriptSig" in {
    val rawScriptSig = TestUtil.rawP2PKScriptSig
    val scriptSig = ScriptSignature(rawScriptSig)
    val result = scriptSig match {
      case s: P2PKScriptSignature => true
      case _ => false
    }
    result must be(true)

  }

  it must "parse a p2sh scriptSignature from a raw scriptSig" in {
    val result = TestUtil.p2shInputScript2Of2 match {
      case s: P2SHScriptSignature => true
      case y => throw new RuntimeException("Should be p2sh input: " + y)
    }
    result must be(true)
  }
}
