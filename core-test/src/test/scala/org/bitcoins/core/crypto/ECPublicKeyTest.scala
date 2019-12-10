package org.bitcoins.core.crypto

import org.bitcoins.testkit.core.gen.CryptoGenerators
import org.bitcoins.testkit.util.BitcoinSUnitTest
import scodec.bits._

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
    forAll(CryptoGenerators.publicKey) { pubKey =>
      val p = pubKey.toPoint
      val pub2 = ECPublicKey.fromPoint(p, pubKey.isCompressed)
      assert(pubKey == pub2)
    }
  }
}
