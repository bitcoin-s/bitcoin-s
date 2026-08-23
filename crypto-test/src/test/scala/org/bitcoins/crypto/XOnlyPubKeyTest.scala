package org.bitcoins.crypto

import scodec.bits.ByteVector

class XOnlyPubKeyTest extends BitcoinSCryptoTest {
  behavior of "XOnlyPubKey"

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  it must "fail for incorrect lengths" in {
    assertThrows[IllegalArgumentException](
      XOnlyPubKey(
        "676f8c22de526e0c0904719847e63bda47b4eceb6986bdbaf8695db362811a"
      )
    )

    assertThrows[IllegalArgumentException](
      XOnlyPubKey(
        "676f8c22de526e0c0904719847e63bda47b4eceb6986bdbaf8695db362811a010203"
      )
    )

    // fromBytes previously silently left-padded inputs shorter than 32 bytes
    // instead of rejecting them -- a 1-byte input like "01" left-padded with
    // zeros becomes the valid but extremely low-entropy x coordinate x=1
    assertThrows[IllegalArgumentException](
      XOnlyPubKey.fromBytes(ByteVector.fromValidHex("01"))
    )

    // fromBytes previously silently dropped the first byte of 33-byte
    // inputs instead of rejecting them
    val validXOnlyHex =
      "79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"
    assertThrows[IllegalArgumentException](
      XOnlyPubKey.fromBytes(ByteVector.fromValidHex("02" + validXOnlyHex))
    )
  }

  it must "fail for invalid x coordinate" in {
    assertThrows[IllegalArgumentException](
      XOnlyPubKey(
        "EEFDEA4CDB677750A420FEE807EACF21EB9898AE79B9768766E4FAA04A2D4A34"
      )
    )

    assertThrows[IllegalArgumentException](
      XOnlyPubKey(
        "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC30"
      )
    )
  }

  it must "succeed for valid large x coordinates above the curve order" in {
    val _ = XOnlyPubKey(
      "fffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2c"
    ).coord
    succeed
  }

  it must "have serialization symmetry" in {
    forAll(CryptoGenerators.xOnlyPubKey) { xOnlyPub =>
      assert(XOnlyPubKey(xOnlyPub.bytes) == xOnlyPub)
      assert(XOnlyPubKey(xOnlyPub.coord) == xOnlyPub)
    }
  }

  it must "correctly go back and forth between x-only and ECPublicKey" in {
    forAll(CryptoGenerators.publicKey) { pubKey =>
      val parity = pubKey.parity
      val xOnly = pubKey.toXOnly

      assert(xOnly.publicKey(parity) == pubKey)
    }
  }

  it must "correctly go back and forth between x-only and SchnorrPubKey" in {
    forAll(CryptoGenerators.schnorrPublicKey) { pubKey =>
      val xOnly = pubKey.toXOnly

      assert(xOnly.schnorrPublicKey == pubKey)
    }
  }
}
