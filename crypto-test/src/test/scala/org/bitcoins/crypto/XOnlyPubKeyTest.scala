package org.bitcoins.crypto

class XOnlyPubKeyTest extends BitcoinSCryptoTest {
  behavior of "XOnlyPubKey"

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  it must "fail for incorrect lengths" in {
    assertThrows[IllegalArgumentException](
      XOnlyPubKey(
        "676f8c22de526e0c0904719847e63bda47b4eceb6986bdbaf8695db362811a"))

    assertThrows[IllegalArgumentException](
      XOnlyPubKey(
        "676f8c22de526e0c0904719847e63bda47b4eceb6986bdbaf8695db362811a010203"))
  }

  it must "fail for invalid x coordinate" in {
    assertThrows[IllegalArgumentException](
      XOnlyPubKey(
        "EEFDEA4CDB677750A420FEE807EACF21EB9898AE79B9768766E4FAA04A2D4A34"))

    assertThrows[IllegalArgumentException](
      XOnlyPubKey(
        "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC30"))
  }

  it must "succeed for valid large x coordinates above the curve order" in {
    val _ = XOnlyPubKey(
      "fffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2c").coord
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
