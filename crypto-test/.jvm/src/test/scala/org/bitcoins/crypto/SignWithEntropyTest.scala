package org.bitcoins.crypto

class SignWithEntropyTest extends BitcoinSCryptoAsyncTest {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  //ECPrivateKey implements the sign interface
  //so just use it for testing purposes
  val privKey: Sign = ECPrivateKey.freshPrivateKey
  val pubKey: ECPublicKey = privKey.publicKey

  behavior of "SignWithEntropy"

  it must "sign arbitrary pieces of data with arbitrary entropy correctly" in {
    forAllAsync(CryptoGenerators.sha256Digest, CryptoGenerators.sha256Digest) {
      case (hash, entropy) =>
        val sigF = privKey.signWithEntropyFunction(hash.bytes, entropy.bytes)

        sigF.map { sig =>
          assert(pubKey.verify(hash.bytes, sig))
        }
    }
  }

}
