package org.bitcoins.core.protocol

import org.bitcoins.core.number.UInt5
import org.bitcoins.core.protocol.script.{WitnessScriptPubKey, WitnessVersion0}
import org.bitcoins.core.util.{Bech32, Bech32Encoding}
import org.bitcoins.core.util.Bech32Encoding.Bech32m
import org.bitcoins.testkitcore.gen._
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

import scala.annotation.tailrec
import scala.util.{Random, Success}

class Bech32mTest extends BitcoinSUnitTest {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  behavior of "Bech32m"

  it must "split all Bech32m addresses into HRP and data" in {
    forAll(AddressGenerator.bech32mAddress) { address =>
      val splitT =
        Bech32.splitToHrpAndData(address.value, Bech32Encoding.Bech32m)
      splitT.isSuccess
    }
  }

  it must "serialization symmetry" in {
    forAll(ScriptGenerators.witnessScriptPubKey.suchThat(
             _._1.witnessVersion != WitnessVersion0),
           ChainParamsGenerator.networkParams) { case ((witSPK, _), network) =>
      val addr = Bech32mAddress(witSPK, network)
      val spk = Bech32mAddress.fromStringToWitSPK(addr.value)
      spk == Success(witSPK)
    }
  }

  it must "checksum must not work if we modify a char" in {
    forAll(AddressGenerator.bech32mAddress) { addr: Bech32mAddress =>
      val old = addr.value
      val rand = Math.abs(Random.nextInt())
      val idx = rand % old.length
      val (f, l) = old.splitAt(idx)
      val replacementChar = pickReplacementChar(l.head)
      val replaced = s"$f$replacementChar${l.tail}"
      //should fail because we replaced a char in the addr, so checksum invalid
      Bech32mAddress.fromStringT(replaced).isFailure
    }
  }

  it must "must fail if we have a mixed case" in {
    forAll(AddressGenerator.bech32mAddress) { addr: Bech32mAddress =>
      val old = addr.value
      val replaced = switchCaseRandChar(old)
      //should fail because we we switched the case of a random char
      val actual = Bech32mAddress.fromStringT(replaced)
      actual.isFailure
    }
  }

  it must "pass valid bech32m test vectors" in {
    assert(Bech32.splitToHrpAndData("A1LQFN3A", Bech32m).isSuccess)
    assert(Bech32.splitToHrpAndData("a1lqfn3a", Bech32m).isSuccess)
    assert(
      Bech32
        .splitToHrpAndData(
          "an83characterlonghumanreadablepartthatcontainsthetheexcludedcharactersbioandnumber11sg7hg6",
          Bech32m)
        .isSuccess)
    assert(
      Bech32
        .splitToHrpAndData(
          "11llllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllludsr8",
          Bech32m)
        .isSuccess)
    assert(Bech32.splitToHrpAndData("?1v759aa", Bech32m).isSuccess)
  }

  it must "pass invalid bech32m test vectors" in {
    assert(
      Bech32
        .splitToHrpAndData(
          "an84characterslonghumanreadablepartthatcontainsthetheexcludedcharactersbioandnumber11d6pts4",
          Bech32m)
        .isFailure)
    assert(Bech32.splitToHrpAndData("qyrz8wqd2c9m", Bech32m).isFailure)
    assert(Bech32.splitToHrpAndData("1qyrz8wqd2c9m", Bech32m).isFailure)
    assert(Bech32.splitToHrpAndData("y1b0jsk6g", Bech32m).isFailure)
    assert(Bech32.splitToHrpAndData("lt1igcx5c0", Bech32m).isFailure)
    assert(Bech32.splitToHrpAndData("in1muywd", Bech32m).isFailure)
    assert(Bech32.splitToHrpAndData("mm1crxm3i", Bech32m).isFailure)
    assert(Bech32.splitToHrpAndData("M1VUXWEZ", Bech32m).isFailure)
    assert(Bech32.splitToHrpAndData("16plkw9", Bech32m).isFailure)
    assert(Bech32.splitToHrpAndData("1p2gdwpf", Bech32m).isFailure)
  }

  it must "get spk from bech32m addresses" in {
    assert(
      Bech32mAddress
        .fromString("BC1PW508D6QEJXTDG4Y5R3ZARVARY0C5XW7KJ9WKRU")
        .scriptPubKey == WitnessScriptPubKey.fromAsmHex(
        "5114751e76e8199196d454941c45d1b3a323f1433bd6"))
    assert(
      Bech32mAddress
        .fromString(
          "tb1prp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3q98lawz")
        .scriptPubKey == WitnessScriptPubKey.fromAsmHex(
        "51201863143c14c5166804bd19203356da136c985678cd4d27a1b8c6329604903262"))
    assert(
      Bech32mAddress
        .fromString(
          "bc1pw508d6qejxtdg4y5r3zarvary0c5xw7kw508d6qejxtdg4y5r3zarvary0c5xw7kt5nd6y")
        .scriptPubKey == WitnessScriptPubKey.fromAsmHex(
        "5128751e76e8199196d454941c45d1b3a323f1433bd6751e76e8199196d454941c45d1b3a323f1433bd6"))
    assert(
      Bech32mAddress
        .fromString("BC1SW50QGDZ25J")
        .scriptPubKey == WitnessScriptPubKey.fromAsmHex("6002751e"))
    assert(
      Bech32mAddress
        .fromString("bc1zw508d6qejxtdg4y5r3zarvaryvaxxpcs")
        .scriptPubKey == WitnessScriptPubKey.fromAsmHex(
        "5210751e76e8199196d454941c45d1b3a323"))
    assert(
      Bech32mAddress
        .fromString(
          "tb1gqqqqp399et2xygdj5xreqhjjvcmzhxw4aywxecjdzew6hylgvsescs2hvq")
        .scriptPubKey == WitnessScriptPubKey.fromAsmHex(
        "5820000000c4a5cad46221b2a187905e5266362b99d5e91c6ce24d165dab93e86433"))
    assert(
      Bech32mAddress
        .fromString(
          "tb1pqqqqp399et2xygdj5xreqhjjvcmzhxw4aywxecjdzew6hylgvsesf3hn0c")
        .scriptPubKey == WitnessScriptPubKey.fromAsmHex(
        "5120000000c4a5cad46221b2a187905e5266362b99d5e91c6ce24d165dab93e86433"))
    assert(
      Bech32mAddress
        .fromString(
          "bc1p0xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7vqzk5jj0")
        .scriptPubKey == WitnessScriptPubKey.fromAsmHex(
        "512079be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"))
  }

  it must "fail to read invalid bech32m addresses" in {
    assert(
      Bech32mAddress
        .fromStringT(
          "tc1p0xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7vq5zuyut")
        .isFailure)
    assert(
      Bech32mAddress
        .fromStringT(
          "bc1p0xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7vqh2y7hd")
        .isFailure)
    assert(
      Bech32mAddress
        .fromStringT(
          "tb1z0xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7vqglt7rf")
        .isFailure)
    assert(
      Bech32mAddress
        .fromStringT(
          "BC1S0XLXVLHEMJA6C4DQV22UAPCTQUPFHLXM9H8Z3K2E72Q4K9HCZ7VQ54WELL")
        .isFailure)
    assert(
      Bech32mAddress
        .fromStringT(
          "bc1p38j9r5y49hruaue7wxjce0updqjuyyx0kh56v8s25huc6995vvpql3jow4")
        .isFailure)
    assert(
      Bech32mAddress
        .fromStringT(
          "BC130XLXVLHEMJA6C4DQV22UAPCTQUPFHLXM9H8Z3K2E72Q4K9HCZ7VQ7ZWS8R")
        .isFailure)
    assert(Bech32mAddress.fromStringT("bc1pw5dgrnzv").isFailure)
    assert(
      Bech32mAddress
        .fromStringT(
          "bc1p0xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7v8n0nx0muaewav253zgeav")
        .isFailure)
    assert(
      Bech32mAddress
        .fromStringT("BC1QR508D6QEJXTDG4Y5R3ZARVARYV98GJ9P")
        .isFailure)
    assert(
      Bech32mAddress
        .fromStringT(
          "tb1p0xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7vq47Zagq")
        .isFailure)
    assert(Bech32mAddress.fromStringT("bc1gmk9yu").isFailure)
  }

  it must "create the correct checksum for a 0 byte address" in {
    val checksum =
      Bech32mAddress.createChecksum(BtcHumanReadablePart.bc, Vector(UInt5.zero))
    checksum must be(Seq(16, 30, 17, 4, 6, 30).map(i => UInt5(i.toByte)))
    checksum.map(ch => Bech32.charset(ch.toInt)).mkString must be("s73yx7")
  }

  it must "fail to read a segwitV0 spk as a bech32m address" in {
    forAll(ScriptGenerators.witnessScriptPubKeyV0,
           ChainParamsGenerator.networkParams) { case (witSpkV0, np) =>
      assert(Bech32mAddress.fromScriptPubKeyT(witSpkV0._1, np).isFailure)
    }
  }

  it must "fail to read a bech32 address" in {
    forAll(AddressGenerator.bech32Address) { bech32 =>
      assert(Bech32mAddress.fromStringT(bech32.toString).isFailure)
    }
  }

  @tailrec
  private def pickReplacementChar(oldChar: Char): Char = {
    val rand = Math.abs(Random.nextInt())
    val newChar = Bech32.charset(rand % Bech32.charset.size)
    //make sure we don't pick the same char we are replacing in the bech32m address
    if (oldChar == newChar) pickReplacementChar(oldChar)
    else newChar
  }

  @tailrec
  private def switchCaseRandChar(addr: String): String = {
    val rand = Math.abs(Random.nextInt())
    val idx = rand % addr.length
    val (f, l) = addr.splitAt(idx)
    if (l.head.isDigit) {
      switchCaseRandChar(addr)
    } else {
      val middle =
        if (l.head.isUpper) {
          l.head.toLower
        } else {
          l.head.toUpper
        }
      s"$f$middle${l.tail}"
    }
  }
}
