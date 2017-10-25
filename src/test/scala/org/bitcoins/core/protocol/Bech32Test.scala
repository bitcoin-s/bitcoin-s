package org.bitcoins.core.protocol

import org.bitcoins.core.config.TestNet3
import org.bitcoins.core.crypto.ECPublicKey
import org.bitcoins.core.number.UInt8
import org.bitcoins.core.protocol.script.WitnessScriptPubKeyV0
import org.scalatest.{FlatSpec, MustMatchers}

import scala.util.Success

class Bech32Test extends FlatSpec with MustMatchers  {

/*  "Bech32" must "validly encode the test vectors from bitcoin core correctly" in {
    val valid = Seq("A12UEL5L",
      "a12uel5l",
      "an83characterlonghumanreadablepartthatcontainsthenumber1andtheexcludedcharactersbio1tt5tgs",
      "abcdef1qpzry9x8gf2tvdw0s3jn54khce6mua7lmqqqxw",
      "11qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqc8247j",
      "split1checkupstagehandshakeupstreamerranterredcaperred2y9e3w",
      "?1ezyfcl"
    )
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

  }*/

  it must "follow the example in BIP173" in {
    //https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki#examples
    val key = ECPublicKey("0279BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798".toLowerCase)
    val p2wpkh = WitnessScriptPubKeyV0(key)
    val expected = Seq(117, 30, 118, 232, 25, 145, 150, 212, 84, 148, 28, 69, 209, 179, 163, 35, 241, 67, 59, 214).map(_.toByte)
    val bytes = p2wpkh.asmBytes.tail.tail
    bytes.size must be (expected.size)
    bytes must be (expected)
    val addr = Bech32Address(p2wpkh,TestNet3)
    addr.map(_.value) must be (Success("tb1qw508d6qejxtdg4y5r3zarvary0c5xw7kxpjzsx"))
  }

  it must "encode 0 byte correctly" in {
    val addr = Bech32Address(bc, Seq(UInt8.zero))
    addr.value must be ("bc1q9zpgru")
  }


  it must "create the correct checksum for a 0 byte address" in {
    val checksum = Bech32Address.createChecksum(bc,Seq(UInt8.zero))
    checksum must be (Seq(5, 2, 1, 8, 3, 28).map(i => UInt8(i.toShort)))
    checksum.map(ch => Bech32Address.charset(ch.underlying)).mkString must be ("9zpgru")
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


  it must "encode base 5 to base 8" in {
    val z = UInt8.zero
    val encoded = "qq"
    Bech32Address.decode(Seq(z,z)) must be (Success(Seq(z)))
  }
}
