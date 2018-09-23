package org.bitcoins.core.protocol

import org.bitcoins.core.gen.{ AddressGenerator, ChainParamsGenerator, ScriptGenerators }
import org.bitcoins.core.util.{ Bech32, BitcoinSLogger }
import org.scalacheck.{ Gen, Prop, Properties }

import scala.annotation.tailrec
import scala.util.{ Random, Success }

class Bech32Spec extends Properties("Bech32Spec") {
  private val logger = BitcoinSLogger.logger

  property("serialization symmetry") = {
    Prop.forAll(ScriptGenerators.witnessScriptPubKey, ChainParamsGenerator.networkParams) {
      case ((witSPK, _), network) =>
        val addr = Bech32Address(witSPK, network)
        val spk = addr.flatMap(a => Bech32Address.fromStringToWitSPK(a.value))
        spk == Success(witSPK)
    }
  }

  property("checksum must not work if we modify a char") = {
    Prop.forAll(AddressGenerator.bech32Address) {
      case addr: Bech32Address =>
        val old = addr.value
        val rand = Math.abs(Random.nextInt)
        val idx = rand % old.size
        val (f, l) = old.splitAt(idx)
        val replacementChar = pickReplacementChar(l.head)
        val replaced = f ++ Seq(replacementChar) ++ l.tail
        //should fail because we replaced a char in the addr, so checksum invalid
        Bech32Address.fromString(replaced).isFailure
    }
  }

  property("must fail if we have a mixed case") = {
    Prop.forAllNoShrink(AddressGenerator.bech32Address) {
      case addr: Bech32Address =>
        val old = addr.value
        val replaced = switchCaseRandChar(old)
        //should fail because we we switched the case of a random char
        val actual = Bech32Address.fromString(replaced)
        actual.isFailure
    }
  }

  @tailrec
  private def pickReplacementChar(oldChar: Char): Char = {
    val rand = Math.abs(Random.nextInt)
    val newChar = Bech32.charset(rand % Bech32.charset.size)
    //make sure we don't pick the same char we are replacing in the bech32 address
    if (oldChar == newChar) pickReplacementChar(oldChar)
    else newChar
  }

  @tailrec
  private def switchCaseRandChar(addr: String): String = {
    val rand = Math.abs(Random.nextInt)
    val idx = rand % addr.size
    val (f, l) = addr.splitAt(idx)
    if (l.head.isDigit) {
      switchCaseRandChar(addr)
    } else if (l.head.isUpper) {
      f ++ Seq(l.head.toLower) ++ l.tail
    } else {
      f ++ Seq(l.head.toUpper) ++ l.tail
    }
  }
}
