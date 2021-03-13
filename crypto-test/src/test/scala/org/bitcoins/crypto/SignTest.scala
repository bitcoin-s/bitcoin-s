package org.bitcoins.crypto

class SignTest extends BitcoinSCryptoAsyncTest {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  //ECPrivateKey implements the sign interface
  //so just use it for testing purposes
  val privKey: Sign = ECPrivateKey.freshPrivateKey
  val pubKey: ECPublicKey = privKey.publicKey

  behavior of "Sign"

  it must "sign arbitrary pieces of data correctly" in {
    forAllAsync(CryptoGenerators.sha256Digest) { hash =>
      val sigF = privKey.signFunction(hash.bytes)

      sigF.map { sig =>
        assert(pubKey.verify(hash.bytes, sig))
      }
    }
  }
}
