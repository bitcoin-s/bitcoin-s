package org.bitcoins.crypto

class SchnorrPublicKeyTest extends BitcoinSCryptoTest {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  behavior of "SchnorrPublicKey"

  it must "fail for incorrect lengths" in {
    assertThrows[IllegalArgumentException](
      SchnorrPublicKey(
        "676f8c22de526e0c0904719847e63bda47b4eceb6986bdbaf8695db362811a"))

    assertThrows[IllegalArgumentException](
      SchnorrPublicKey(
        "676f8c22de526e0c0904719847e63bda47b4eceb6986bdbaf8695db362811a010203"))
  }

  it must "fail for invalid x coordinate" in {
    assertThrows[IllegalArgumentException](
      SchnorrPublicKey(
        "EEFDEA4CDB677750A420FEE807EACF21EB9898AE79B9768766E4FAA04A2D4A34"))

    assertThrows[IllegalArgumentException](
      SchnorrPublicKey(
        "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC30"))
  }

  it must "succeed for valid large x coordinates above the curve order" in {
    val _ = SchnorrPublicKey(
      "fffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2c").xCoord
    succeed
  }

  it must "have serialization symmetry" in {
    forAll(CryptoGenerators.schnorrPublicKey) { pubKey =>
      assert(SchnorrPublicKey(pubKey.bytes) == pubKey)
      assert(SchnorrPublicKey(pubKey.xCoord) == pubKey)
    }
  }

}
