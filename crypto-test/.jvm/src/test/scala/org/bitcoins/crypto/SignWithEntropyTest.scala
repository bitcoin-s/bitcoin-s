package org.bitcoins.crypto

import scodec.bits.ByteVector

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt

class SignWithEntropyTest extends BitcoinSCryptoTest {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  // ECPrivateKey implements the sign interface
  // so just use it for testing purposes
  val privKey: Sign = ECPrivateKey.freshPrivateKey
  val pubKey: ECPublicKey = privKey.publicKey

  behavior of "SignWithEntropy"

  it must "sign arbitrary data correctly with low R values" in {
    forAll(CryptoGenerators.sha256Digest) { hash =>
      val bytes = hash.bytes

      val sig1 = privKey.signLowR(bytes)
      val sig2 = privKey.signLowR(bytes) // Check for determinism
      assert(pubKey.verify(bytes, sig1))
      assert(
        sig1.bytes.length <= 70
      ) // This assertion fails if Low R is not used
      assert(sig1.bytes == sig2.bytes)
      assert(sig1 == sig2)
    }
  }

  it must "grind for low R the way Bitcoin Core does" in {
    // r has its top bit set but s is short, so the DER encoding is still
    // 70 bytes: Core keeps grinding, since it tests r alone (SigHasLowR)
    val vectors = Vector(
      (
        "9b0ca5da766ee3d0aadb01ad2f4b74ee5a68b9d515c4a5b2ff33e0969ce793be",
        "d6a76f7c5cb01051a8e5dc22da608a3a7e30116adc3bc56e7782d048f8c72da5",
        "304402202cebda88731b39876f1d916fb0eb1ad6f152119537e94fd123a6e67c148eae94022000da1c62fd481b1bb1600eebd7edc491cfa78e51d298314abc46f9bde0b19d03"
      ),
      (
        "35da9b227367aa6643e850343e59f270ebbbe907d48fddd84ec14f500de42573",
        "817cb899cc0f7218caad21fcd0ed1f7f3da3777585eef988f4a0f51fee9e4b21",
        "3044022010c0793be9b91d6093595e56db0735d9089ab90400e0fbb83ff86674bcd86fa802206ecdbcee2d37bf97e18ef84690d28a36dc1c24671eb78b9d81d0cbff74fd4fe8"
      )
    )
    vectors.foreach { case (keyHex, hashHex, sigHex) =>
      val key = ECPrivateKey.fromHex(keyHex)
      val hash = ByteVector.fromValidHex(hashHex)
      val expected = ECDigitalSignature.fromHex(sigHex)
      val asyncSign =
        AsyncSign(key.asyncSign, key.asyncSignWithEntropy, key.publicKey)

      assert(key.signLowR(hash) == expected)
      assert(Await.result(asyncSign.asyncSignLowR(hash), 5.seconds) == expected)
    }
  }

  it must "sign arbitrary pieces of data with arbitrary entropy correctly" in {
    forAll(CryptoGenerators.sha256Digest, CryptoGenerators.sha256Digest) {
      case (hash, entropy) =>
        val sig = privKey.signWithEntropy(hash.bytes, entropy.bytes)

        assert(pubKey.verify(hash.bytes, sig))
    }
  }
}
