package org.bitcoins.crypto

import scodec.bits.ByteVector

class SignTest extends BitcoinSCryptoTest {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  //ECPrivateKey implements the sign interface
  //so just use it for testing purposes
  val privKey: Sign = ECPrivateKey.freshPrivateKey
  val pubKey: ECPublicKey = privKey.publicKey

  behavior of "Sign"

  it must "identify DER encoded signatures" in {
    val privateKey = ECPrivateKey.fromHex(
      "109cc9befbae8ff3e8b342f08091bf6e6a36d2b6e7acfa9b477d9abc71eb94b4")
    val publicKey = privateKey.publicKey
    val data = ByteVector.fromValidHex(
      "258592575c6bd6d489c38662199f3d469fa9296b56d5873159eb66775035919e")
    val sigCompact = ECDigitalSignature.fromHex(
      "b4b07c3373ec271ccf0c51e7491f6d82cdc7b0c50f8ea11130fa266348824a2a2e3f190a4d139faa3e17c677cd8fdcc96c51aee7c1cb2e6edb1d6f8637063f20")
    val sigDER = ECDigitalSignature.fromHex(
      "3045022100b4b07c3373ec271ccf0c51e7491f6d82cdc7b0c50f8ea11130fa266348824a2a02202e3f190a4d139faa3e17c677cd8fdcc96c51aee7c1cb2e6edb1d6f8637063f20")
    assert(DERSignatureUtil.isDEREncoded(sigDER))
    assert(!DERSignatureUtil.isDEREncoded(sigCompact))
    assert(publicKey.verify(data, sigDER))
  }

  it must "sign arbitrary pieces of data correctly" in {
    forAll(CryptoGenerators.sha256Digest) { hash =>
      val sig = privKey.sign(hash.bytes)

      assert(pubKey.verify(hash.bytes, sig))
    }
  }
}
