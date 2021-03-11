package org.bitcoins.crypto

class SchnorrNonceTest extends BitcoinSCryptoTest {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  behavior of "SchnorrNonce"

  it must "fail for incorrect lengths" in {
    assertThrows[IllegalArgumentException](
      SchnorrNonce(
        "676f8c22de526e0c0904719847e63bda47b4eceb6986bdbaf8695db362811a"))

    assertThrows[IllegalArgumentException](
      SchnorrNonce(
        "676f8c22de526e0c0904719847e63bda47b4eceb6986bdbaf8695db362811a010203"))
  }

  it must "fail for invalid x coordinate" in {
    assertThrows[IllegalArgumentException](
      SchnorrNonce(
        "EEFDEA4CDB677750A420FEE807EACF21EB9898AE79B9768766E4FAA04A2D4A34"))

    assertThrows[IllegalArgumentException](
      SchnorrNonce(
        "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC30"))
  }

  it must "have serialization symmetry" in {
    forAll(CryptoGenerators.schnorrNonce) { pubKey =>
      assert(SchnorrNonce(pubKey.bytes) == pubKey)
      assert(SchnorrNonce(pubKey.xCoord) == pubKey)
    }
  }
}
