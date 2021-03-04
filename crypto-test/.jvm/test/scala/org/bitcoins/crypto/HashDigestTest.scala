package org.bitcoins.crypto

import org.bitcoins.testkitcore.gen.{CryptoGenerators, NumberGenerator}
import org.bitcoins.testkitcore.util.BitcoinSUnitTest
import scodec.bits._

class HashDigestTest extends BitcoinSUnitTest {
  behavior of "DoubleSha256Digest"

  it must "be constructable from 32 bytes" in {
    forAll(NumberGenerator.bytes(32)) { bytes =>
      val vec = ByteVector(bytes)
      assert(DoubleSha256Digest(vec).bytes == vec)
    }
  }

  it must "not be constructable from bad byte lenghts" in {
    forAll(NumberGenerator.bytevector.suchThat(_.length != 32)) { bytes =>
      intercept[IllegalArgumentException] {
        DoubleSha256Digest(bytes)
      }
    }
  }

  it must "have flip symmetry" in {
    forAll(CryptoGenerators.doubleSha256Digest) { hash =>
      val flipped = hash.flip
      assert(flipped.flip == hash)
    }
  }

  behavior of "DoubleSha256DigestBE"
  it must "be constructable from 32 bytes" in {
    forAll(NumberGenerator.bytes(32)) { bytes =>
      val vec = ByteVector(bytes)
      assert(DoubleSha256DigestBE(vec).bytes == vec)
    }
  }

  it must "not be constructable from bad byte lenghts" in {
    forAll(NumberGenerator.bytevector.suchThat(_.length != 32)) { bytes =>
      intercept[IllegalArgumentException] {
        DoubleSha256DigestBE(bytes)
      }
    }
  }

}
