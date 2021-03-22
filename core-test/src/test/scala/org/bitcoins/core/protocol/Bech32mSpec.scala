package org.bitcoins.core.protocol

import org.bitcoins.core.protocol.script.WitnessVersion0
import org.bitcoins.core.util.{Bech32, Bech32Encoding}
import org.bitcoins.testkitcore.gen._
import org.scalacheck.{Prop, Properties}

import scala.annotation.tailrec
import scala.util.{Random, Success}

class Bech32mSpec extends Properties("Bech32mSpec") {

  property("split all Bech32m addresses into HRP and data") = {
    Prop.forAll(AddressGenerator.bech32mAddress) { address =>
      val splitT =
        Bech32.splitToHrpAndData(address.value, Bech32Encoding.Bech32m)
      splitT.isSuccess
    }
  }

  property("serialization symmetry") = {
    Prop.forAll(ScriptGenerators.witnessScriptPubKey.suchThat(
                  _._1.witnessVersion != WitnessVersion0),
                ChainParamsGenerator.networkParams) {
      case ((witSPK, _), network) =>
        val addr = Bech32mAddress(witSPK, network)
        val spk = Bech32mAddress.fromStringToWitSPK(addr.value)
        spk == Success(witSPK)
    }
  }

  property("checksum must not work if we modify a char") = {
    Prop.forAll(AddressGenerator.bech32mAddress) { addr: Bech32mAddress =>
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

  property("must fail if we have a mixed case") = {
    Prop.forAllNoShrink(AddressGenerator.bech32mAddress) {
      addr: Bech32mAddress =>
        val old = addr.value
        val replaced = switchCaseRandChar(old)
        //should fail because we we switched the case of a random char
        val actual = Bech32mAddress.fromStringT(replaced)
        actual.isFailure
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
