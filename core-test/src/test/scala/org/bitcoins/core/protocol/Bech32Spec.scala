package org.bitcoins.core.protocol

import org.bitcoins.core.util.Bech32
import org.bitcoins.testkitcore.gen.ln.LnInvoiceGen
import org.bitcoins.testkitcore.gen.{
  AddressGenerator,
  ChainParamsGenerator,
  ScriptGenerators
}
import org.scalacheck.{Prop, Properties}

import scala.annotation.tailrec
import scala.util.{Random, Success}

class Bech32Spec extends Properties("Bech32Spec") {
  property("split all LN invoices into HRP and data") = {
    Prop.forAll(LnInvoiceGen.lnInvoice) { invoice =>
      val splitT = Bech32.splitToHrpAndData(invoice.toString)
      splitT.isSuccess
    }
  }

  property("split all Bech32 addresses into HRP and data") = {
    Prop.forAll(AddressGenerator.bech32Address) { address =>
      val splitT = Bech32.splitToHrpAndData(address.value)
      splitT.isSuccess
    }
  }

  property("serialization symmetry") = {
    Prop.forAll(ScriptGenerators.witnessScriptPubKeyV0,
                ChainParamsGenerator.networkParams) {
      case ((witSPK, _), network) =>
        val addr = Bech32Address(witSPK, network)
        val spk = Bech32Address.fromStringToWitSPK(addr.value)
        spk == Success(witSPK)
    }
  }

  property("checksum must not work if we modify a char") = {
    Prop.forAll(AddressGenerator.bech32Address) { addr: Bech32Address =>
      val old = addr.value
      val rand = Math.abs(Random.nextInt())
      val idx = rand % old.length
      val (f, l) = old.splitAt(idx)
      val replacementChar = pickReplacementChar(l.head)
      val replaced = s"$f$replacementChar${l.tail}"
      //should fail because we replaced a char in the addr, so checksum invalid
      Bech32Address.fromStringT(replaced).isFailure
    }
  }

  property("must fail if we have a mixed case") = {
    Prop.forAllNoShrink(AddressGenerator.bech32Address) { addr: Bech32Address =>
      val old = addr.value
      val replaced = switchCaseRandChar(old)
      //should fail because we we switched the case of a random char
      val actual = Bech32Address.fromStringT(replaced)
      actual.isFailure
    }
  }

  @tailrec
  private def pickReplacementChar(oldChar: Char): Char = {
    val rand = Math.abs(Random.nextInt())
    val newChar = Bech32.charset(rand % Bech32.charset.size)
    //make sure we don't pick the same char we are replacing in the bech32 address
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
