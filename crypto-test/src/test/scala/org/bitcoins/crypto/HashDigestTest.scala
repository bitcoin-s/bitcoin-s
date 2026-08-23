package org.bitcoins.crypto

import scodec.bits._

class HashDigestTest extends BitcoinSCryptoTest {
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

  behavior of "Sha1Digest"

  it must "reject wrong-length inputs for Sha1Digest and Sha1DigestBE" in {
    // Sha1Digest.fromBytes and Sha1DigestBE.fromBytes previously had no
    // length checks, unlike every other hash digest type in the same file
    // (Sha256Digest, RipeMd160Digest, Sha256Hash160Digest, Sha3_256Digest
    // all validate their expected length).
    val nineteenBytes = ByteVector(Array.fill(19)(1.toByte))
    val twentyBytes = ByteVector(Array.fill(20)(1.toByte))
    val twentyOneBytes = ByteVector(Array.fill(21)(1.toByte))

    // positive control: exactly 20 bytes must keep working
    Sha1Digest.fromBytes(twentyBytes).bytes must be(twentyBytes)
    Sha1DigestBE.fromBytes(twentyBytes).bytes must be(twentyBytes)

    intercept[IllegalArgumentException] {
      Sha1Digest.fromBytes(nineteenBytes)
    }
    intercept[IllegalArgumentException] {
      Sha1Digest.fromBytes(twentyOneBytes)
    }
    intercept[IllegalArgumentException] {
      Sha1DigestBE.fromBytes(nineteenBytes)
    }
    intercept[IllegalArgumentException] {
      Sha1DigestBE.fromBytes(twentyOneBytes)
    }
  }

}
