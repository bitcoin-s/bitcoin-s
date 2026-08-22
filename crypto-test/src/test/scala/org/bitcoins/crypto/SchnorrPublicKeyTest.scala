package org.bitcoins.crypto

import scodec.bits.ByteVector

class SchnorrPublicKeyTest extends BitcoinSCryptoTest {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  behavior of "SchnorrPublicKey"

  it must "fail for incorrect lengths" in {
    assertThrows[IllegalArgumentException](
      SchnorrPublicKey(
        "676f8c22de526e0c0904719847e63bda47b4eceb6986bdbaf8695db362811a"
      )
    )

    assertThrows[IllegalArgumentException](
      SchnorrPublicKey(
        "676f8c22de526e0c0904719847e63bda47b4eceb6986bdbaf8695db362811a010203"
      )
    )

    // fromBytes previously silently left-padded inputs shorter than 32 bytes
    // instead of rejecting them -- a 1-byte input like "01" left-padded with
    // zeros becomes the valid but extremely low-entropy x coordinate x=1
    assertThrows[IllegalArgumentException](
      SchnorrPublicKey.fromBytes(ByteVector.fromValidHex("01"))
    )

    // fromBytes previously silently dropped the first byte of 33-byte
    // inputs instead of rejecting them
    val validXOnlyHex =
      "79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"
    assertThrows[IllegalArgumentException](
      SchnorrPublicKey.fromBytes(ByteVector.fromValidHex("02" + validXOnlyHex))
    )
  }

  it must "fail for invalid x coordinate" in {
    assertThrows[IllegalArgumentException](
      SchnorrPublicKey(
        "EEFDEA4CDB677750A420FEE807EACF21EB9898AE79B9768766E4FAA04A2D4A34"
      )
    )

    assertThrows[IllegalArgumentException](
      SchnorrPublicKey(
        "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC30"
      )
    )
  }

  it must "succeed for valid large x coordinates above the curve order" in {
    val _ = SchnorrPublicKey(
      "fffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2c"
    ).xCoord
    succeed
  }

  it must "have serialization symmetry" in {
    forAll(CryptoGenerators.schnorrPublicKey) { pubKey =>
      assert(SchnorrPublicKey(pubKey.bytes) == pubKey)
      assert(SchnorrPublicKey(pubKey.xCoord) == pubKey)
    }
  }

}
