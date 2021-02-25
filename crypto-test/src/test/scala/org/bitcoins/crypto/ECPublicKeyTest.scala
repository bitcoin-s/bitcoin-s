package org.bitcoins.crypto

import org.bitcoins.testkit.core.gen.CryptoGenerators
import org.bitcoins.testkit.util.BitcoinSUnitTest
import scodec.bits._

import scala.concurrent.ExecutionContext

class ECPublicKeyTest extends BitcoinSUnitTest {

  it must "be able to decompress keys" in {
    val uncompressed =
      ECPublicKey(
        hex"044f355bdcb7cc0af728ef3cceb9615d90684bb5b2ca5f859ab0f0b704075871aa385b6b1b8ead809ca67454d9683fcf2ba03456d6fe2c4abe2b07f0fbdbb2f1c1")
    val compressed =
      ECPublicKey(
        hex"034f355bdcb7cc0af728ef3cceb9615d90684bb5b2ca5f859ab0f0b704075871aa")

    assert(uncompressed.isFullyValid)
    assert(compressed.isFullyValid)

    assert(compressed.isCompressed)
    assert(!uncompressed.isCompressed)

    assert(compressed.decompressed == uncompressed)
    assert(uncompressed.decompressed == uncompressed)
  }

  it must "generate unique keys" in {
    assert(ECPublicKey() != ECPublicKey())
  }

  it must "have serialization symmetry from ECPublicKey -> ECPoint -> ECPublicKey" in {
    CryptoContext.cryptoRuntime match {
      case _: BouncycastleCryptoRuntime | _: LibSecp256k1CryptoRuntime =>
        forAll(CryptoGenerators.publicKey) { pubKey =>
          val p = BouncyCastleUtil.decodePoint(pubKey)
          val pub2 = BouncyCastleUtil.decodePubKey(p, pubKey.isCompressed)
          assert(pubKey == pub2)
        }
      case _ => succeed
    }
  }

  it must "decompress keys correctly" in {
    forAll(CryptoGenerators.privateKey) { privKey =>
      val pubKey = privKey.publicKey

      assert(privKey.isCompressed)
      assert(pubKey.isCompressed)

      val decompressedPrivKey =
        ECPrivateKey(privKey.bytes, isCompressed = false)(
          ExecutionContext.global)
      val decompressedPubKey = pubKey.decompressed

      assert(decompressedPrivKey.publicKey == decompressedPubKey)
      assert(pubKey.bytes.tail == decompressedPubKey.bytes.splitAt(33)._1.tail)
    }
  }

  it must "fail if given invalid pubkey" in {
    // 31 bytes
    val badHex =
      "02020202020202020202020202020202020202020202020202020202020202"
    assert(!ECPublicKey.isFullyValid(ByteVector.fromHex(badHex).get))
  }
}
