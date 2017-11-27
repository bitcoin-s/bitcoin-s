package org.bitcoins.core.protocol

import org.bitcoins.core.config.{MainNet, TestNet3}
import org.bitcoins.core.crypto.ECPublicKey
import org.bitcoins.core.number.UInt8
import org.bitcoins.core.protocol.script.{P2PKHScriptPubKey, P2PKScriptPubKey, WitnessScriptPubKey, WitnessScriptPubKeyV0}
import org.scalatest.{FlatSpec, MustMatchers}

import scala.util.{Success, Try}

class Bech32Test extends FlatSpec with MustMatchers  {

  "Bech32" must "validly encode the test vectors from bitcoin core correctly" in {
    val valid = Seq("A12UEL5L",
      "a12uel5l",
      "an83characterlonghumanreadablepartthatcontainsthenumber1andtheexcludedcharactersbio1tt5tgs",
      "abcdef1qpzry9x8gf2tvdw0s3jn54khce6mua7lmqqqxw",
      "11qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqc8247j",
      "split1checkupstagehandshakeupstreamerranterredcaperred2y9e3w",
      "?1ezyfcl"
    )
    val results: Seq[Try[(HumanReadablePart,Seq[Byte])]] = valid.map(Bech32Address.fromString(_))
    results.exists(_.isFailure) must be (false)
  }

  it must "mark invalid test vectors as invalid from bitcoin core" in {
    val invalid = Seq(" 1nwldj5",
    "\\x7f\"\"1axkwrx",
    "\\x80\"\"1eym55h",
    "an84characterslonghumanreadablepartthatcontainsthenumber1andtheexcludedcharactersbio1569pvx",
    "pzry9x0s0muk",
    "1pzry9x0s0muk",
    "x1b4n0q5v",
    "li1dgmt3",
    "de1lg7wt\\xff",
    "A1G7SGD8",
    "10a06t8",
    "1qzzfhee")
    val results: Seq[Try[(HumanReadablePart,Seq[Byte])]] = invalid.map(Bech32Address.fromString(_))
    results.exists(_.isSuccess) must be (false)
  }

  it must "follow the example in BIP173" in {
    //https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki#examples
    val key = ECPublicKey("0279BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798".toLowerCase)
    val p2wpkh = WitnessScriptPubKeyV0(key)
    val addr = Bech32Address(p2wpkh,TestNet3)
    addr.map(_.value) must be (Success("tb1qw508d6qejxtdg4y5r3zarvary0c5xw7kxpjzsx"))

    //decode
    val decoded = addr.flatMap(a => Bech32Address.fromStringToWitSPK(a.value))
    decoded must be (Success(p2wpkh))

    val p2wpkhMain = Bech32Address(p2wpkh,MainNet)
    p2wpkhMain.map(_.value) must be (Success("bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4"))

    val mp2wpkhDecoded = p2wpkhMain.flatMap(a => Bech32Address.fromStringToWitSPK(a.value))
    mp2wpkhDecoded must be (Success(p2wpkh))

    val p2pk = P2PKScriptPubKey(key)
    val p2wsh = WitnessScriptPubKeyV0(p2pk)
    val addr1 = Bech32Address(p2wsh,TestNet3)
    addr1.map(_.value) must be (Success("tb1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3q0sl5k7"))

    //decode
    val decoded1 = addr1.flatMap(a => Bech32Address.fromStringToWitSPK(a.value))
    decoded1 must be (Success(p2wsh))

    val p2wshMain = Bech32Address(p2wsh,MainNet)
    p2wshMain.map(_.value) must be (Success("bc1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3qccfmv3"))
    val mp2wshDecoded = p2wshMain.flatMap(a => Bech32Address.fromStringToWitSPK(a.value))
    mp2wshDecoded must be (Success(p2wsh))
  }

  it must "encode 0 byte correctly" in {
    val addr = Bech32Address(bc, Seq(UInt8.zero))
    addr.value must be ("bc1q9zpgru")
  }


  it must "create the correct checksum for a 0 byte address" in {
    val checksum = Bech32Address.createChecksum(bc,Seq(UInt8.zero))
    checksum must be (Seq(5, 2, 1, 8, 3, 28).map(i => UInt8(i.toShort)))
    checksum.map(ch => Bech32Address.charset(ch.toInt)).mkString must be ("9zpgru")
  }

  it must "encode base 8 to base 5" in {
    val z = UInt8.zero
    val encoded = Bech32Address.encode(Seq(z))
    encoded.map(Bech32Address.encodeToString(_)) must be (Success("qq"))

    val encoded1 = Bech32Address.encode(Seq(z, UInt8.one))
    encoded1 must be (Success(Seq(z,z,z,UInt8(16.toShort))))
    //130.toByte == -126
    val encoded2 = Bech32Address.encode(Seq(130).map(i => UInt8(i.toShort)))
    encoded2 must be (Success(Seq(16,8).map(i => UInt8(i.toShort))))

    //130.toByte == -126
    val encoded3 = Bech32Address.encode(Seq(255,255).map(i => UInt8(i.toShort)))
    encoded3 must be (Success(Seq(31, 31, 31, 16).map(i => UInt8(i.toShort))))

    val encoded4 = Bech32Address.encode(Seq(255,255,255,255).map(i => UInt8(i.toShort)))
    encoded4 must be (Success(Seq(31, 31, 31, 31, 31, 31, 24).map(i => UInt8(i.toShort))))

    val encoded5 = Bech32Address.encode(Seq(255,255,255,255,255).map(i => UInt8(i.toShort)))
    encoded5 must be (Success(Seq(31, 31, 31, 31, 31, 31, 31, 31).map(i => UInt8(i.toShort))))

    val encoded6 = Bech32Address.encode(Seq(255,255,255,255,255,255).map(i => UInt8(i.toShort)))
    encoded6 must be (Success(Seq(31, 31, 31, 31, 31, 31, 31, 31, 31, 28).map(i => UInt8(i.toShort))))
  }
}
