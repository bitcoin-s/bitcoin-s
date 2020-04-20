package org.bitcoins.core.protocol

import org.bitcoins.core.config.{MainNet, TestNet3}
import org.bitcoins.core.crypto.ECPublicKey
import org.bitcoins.core.number.{UInt5, UInt8}
import org.bitcoins.core.protocol.ln.LnHumanReadablePart
import org.bitcoins.core.protocol.ln.currency.PicoBitcoins
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.util.Bech32
import org.bitcoins.testkit.core.gen.NumberGenerator
import org.bitcoins.testkit.util.BitcoinSUnitTest
import scodec.bits.ByteVector

import scala.util.{Failure, Success}

class Bech32Test extends BitcoinSUnitTest {
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  behavior of "Bech32"

  it must "decode a regtest address from Bitcoin Core" in {
    val addrStr = "bcrt1qq6w6pu6zq90az9krn53zlkvgyzkyeglzukyepf"
    val addrT = Address.fromString(addrStr)
    addrT match {
      case Success(addr: Bech32Address) => assert(addr.value == addrStr)
      case _                            => fail()
    }
  }

  it must "follow the example in BIP173" in {
    //https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki#examples
    val key = ECPublicKey(
      "0279BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798".toLowerCase)
    val p2wpkh = P2WPKHWitnessSPKV0(key)
    val addr = Bech32Address(p2wpkh, TestNet3)
    addr.value must be("tb1qw508d6qejxtdg4y5r3zarvary0c5xw7kxpjzsx")

    //decode
    val decoded = Bech32Address.fromStringToWitSPK(addr.value)
    decoded must be(Success(p2wpkh))

    val p2wpkhMain = Bech32Address(p2wpkh, MainNet)
    p2wpkhMain.value must be("bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4")

    val mp2wpkhDecoded = Bech32Address.fromStringToWitSPK(p2wpkhMain.value)
    mp2wpkhDecoded must be(Success(p2wpkh))

    val p2pk = P2PKScriptPubKey(key)
    val p2wsh = P2WSHWitnessSPKV0(p2pk)
    val addr1 = Bech32Address(p2wsh, TestNet3)
    addr1.value must be(
      "tb1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3q0sl5k7")

    //decode
    val decoded1 = Bech32Address.fromStringToWitSPK(addr1.value)
    decoded1 must be(Success(p2wsh))

    val p2wshMain = Bech32Address(p2wsh, MainNet)
    p2wshMain.value must be(
      "bc1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3qccfmv3")
    val mp2wshDecoded = Bech32Address.fromStringToWitSPK(p2wshMain.value)
    mp2wshDecoded must be(Success(p2wsh))
  }

  it must "expand the human readable part correctly - BTC" in {
    BtcHumanReadablePart.bc.expand must be(
      Vector(UInt5(3), UInt5(3), UInt5(0), UInt5(2), UInt5(3)))

    BtcHumanReadablePart.tb.expand must be(
      Vector(UInt5(3), UInt5(3), UInt5(0), UInt5(20), UInt5(2)))

    BtcHumanReadablePart.bcrt.expand must be(
      Vector(UInt5(3),
             UInt5(3),
             UInt5(3),
             UInt5(3),
             UInt5(0),
             UInt5(2),
             UInt5(3),
             UInt5(18),
             UInt5(20)))

  }
  it must "expand the human readable part correctly - LN no amount" in {
    LnHumanReadablePart.lnbc(None).expand must be(
      Vector(
        UInt5(3),
        UInt5(3),
        UInt5(3),
        UInt5(3),
        UInt5(0),
        UInt5(12),
        UInt5(14),
        UInt5(2),
        UInt5(3)
      ))

    LnHumanReadablePart.lntb(None).expand must be(
      Vector(
        UInt5(3),
        UInt5(3),
        UInt5(3),
        UInt5(3),
        UInt5(0),
        UInt5(12),
        UInt5(14),
        UInt5(20),
        UInt5(2)
      ))

    LnHumanReadablePart.lnbcrt(None).expand must be(
      Vector(
        UInt5(3),
        UInt5(3),
        UInt5(3),
        UInt5(3),
        UInt5(3),
        UInt5(3),
        UInt5(0),
        UInt5(12),
        UInt5(14),
        UInt5(2),
        UInt5(3),
        UInt5(18),
        UInt5(20)
      ))
  }

  it must "expand the human readable part correctly - LN with amount" in {
    val hrp = LnHumanReadablePart.lnbc(Some(PicoBitcoins(724)))
    val expanded = hrp.expand
    assert(
      expanded == Vector(
        UInt5(3),
        UInt5(3),
        UInt5(3),
        UInt5(3),
        UInt5(1),
        UInt5(1),
        UInt5(1),
        UInt5(3),
        UInt5(0),
        UInt5(12),
        UInt5(14),
        UInt5(2),
        UInt5(3),
        UInt5(23),
        UInt5(18),
        UInt5(20),
        UInt5(16)
      ))
  }

  it must "encode 0 byte correctly" in {
    val addr = Bech32Address(BtcHumanReadablePart.bc, Vector(UInt5.zero))
    addr.value must be("bc1q9zpgru")
  }

  it must "create the correct checksum for a 0 byte address" in {
    val checksum =
      Bech32Address.createChecksum(BtcHumanReadablePart.bc, Vector(UInt5.zero))
    checksum must be(Seq(5, 2, 1, 8, 3, 28).map(i => UInt5(i.toByte)))
    checksum.map(ch => Bech32.charset(ch.toInt)).mkString must be("9zpgru")
  }

  it must "encode from uint8 to uint5" in {
    val z = UInt8.zero
    val fz = UInt5.zero
    val encoded = Bech32.from8bitTo5bit(Vector(z))

    Bech32.encode5bitToString(encoded) must be("qq")

    val eightBitEncoded = Bech32.from5bitTo8bit(encoded)
    Bech32.encode8bitToString(eightBitEncoded) must be("qq")
    val byteVectorEncoded =
      eightBitEncoded.foldLeft(ByteVector.empty)(_ ++ _.bytes)
    Bech32.encode8bitToString(byteVectorEncoded) must be("qq")

    val encoded1 = Bech32.from8bitTo5bit(Vector(z, UInt8.one))
    encoded1 must be(Seq(fz, fz, fz, UInt5(16.toByte)))
    //130.toByte == -126
    val encoded2 =
      Bech32.from8bitTo5bit(Vector(130).map(i => UInt8(i.toShort)))
    encoded2 must be(Seq(16, 8).map(i => UInt5(i.toByte)))

    //130.toByte == -126
    val encoded3 =
      Bech32.from8bitTo5bit(Vector(255, 255).map(i => UInt8(i.toShort)))
    encoded3 must be(Seq(31, 31, 31, 16).map(i => UInt5(i.toByte)))

    val encoded4 = Bech32.from8bitTo5bit(
      Vector(255, 255, 255, 255).map(i => UInt8(i.toShort)))
    encoded4 must be(Seq(31, 31, 31, 31, 31, 31, 24).map(i => UInt5(i.toByte)))

    val encoded5 = Bech32.from8bitTo5bit(
      Vector(255, 255, 255, 255, 255).map(i => UInt8(i.toShort)))
    encoded5 must be(
      Seq(31, 31, 31, 31, 31, 31, 31, 31).map(i => UInt5(i.toByte)))

    val encoded6 = Bech32.from8bitTo5bit(
      Vector(255, 255, 255, 255, 255, 255).map(i => UInt8(i.toByte)))

    encoded6 must be(
      Seq(31, 31, 31, 31, 31, 31, 31, 31, 31, 28).map(i => UInt5(i.toByte)))
  }

  it must "encode from 8 bit to 5 bit and back" in {
    forAll(NumberGenerator.uInt8s) { u8s =>
      val u5s = Bech32.from8bitTo5bit(u8s.toVector)
      val u8sAgain = Bech32.from5bitTo8bit(u5s)
      assert(u8s == u8sAgain)
    }
  }

  // bech 32 Weakness test vectors
  private val invalidBech32 = Vector(
    "bc1qvfu057ptzatpxf30xc2yunuqad4dg54gkuvx04fuzch6gezltk2q63lcaqp",
    "bc1ql6mqttdzpekxmdp9gaglvjtnfg4ydwzdtgcqxxru7f6m0eg9pckqnncp",
    "bc1q6g6y8y62cgt7x3s2sdwax3nuur00uwcey570zqqp",
    "bc1q3qwt6j7yr9nzhskdh36eh6ktesy93ggwjfs9p"
  )
  it must "fail to find the bech32 weakness" in {
    val failsAll = invalidBech32.forall(invalid =>
      Bech32.splitToHrpAndData(invalid).isSuccess)
    assert(failsAll)
  }

  it must "fail hrp if out of range" in {
    assert(Bech32.checkHrpValidity(s"${32.toChar}").isFailure)
    assert(Bech32.checkHrpValidity(s"${127.toChar}").isFailure)
  }

  it must "fail if character is not in charset" in {
    // b is not allowed in data
    assert(
      Bech32
        .checkDataValidity("bcrt1qq6w6pu6zq90az9krn53zlkvgyzkyeglzukyepf")
        .isFailure)
  }
}
