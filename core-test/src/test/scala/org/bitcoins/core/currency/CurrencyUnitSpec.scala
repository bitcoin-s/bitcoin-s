package org.bitcoins.core.currency

import org.bitcoins.core.gen.CurrencyUnitGenerator
import org.bitcoins.core.number.Int64
import org.scalacheck.{Gen, Prop, Properties}

import scala.util.Try

/**
  * Created by chris on 6/23/16.
  */
class CurrencyUnitSpec extends Properties("CurrencyUnitSpec") {

  private val satoshiWithInt: Gen[(Satoshis, Int)] = for {
    sat <- CurrencyUnitGenerator.satoshis
    num <- Gen.choose(Int.MinValue, Int.MaxValue)
  } yield (sat, num)

  property("Symmetrical serialization for satoshis") =
    Prop.forAll(CurrencyUnitGenerator.satoshis) { satoshis =>
      Satoshis(satoshis.hex) == satoshis
    }

  property("additive identity") = Prop.forAll(CurrencyUnitGenerator.satoshis) {
    satoshis =>
      satoshis + CurrencyUnits.zero == satoshis
  }

  property("add two satoshis") = Prop.forAll(CurrencyUnitGenerator.satoshis,
                                             CurrencyUnitGenerator.satoshis) {
    (num1: Satoshis, num2: Satoshis) =>
      val result: Try[Int64] = Try(Int64(num1.toBigInt + num2.toBigInt))
      if (result.isSuccess && result.get >= Int64(Satoshis.min.toLong) &&
          result.get <= Int64(Satoshis.max.toLong))
        num1 + num2 == Satoshis(result.get)
      else Try(num1 + num2).isFailure
  }

  property("Subtractive identity for satoshis") =
    Prop.forAll(CurrencyUnitGenerator.satoshis) { satoshis =>
      satoshis - CurrencyUnits.zero == satoshis
    }

  property("Subtract two satoshi values") = Prop.forAll(
    CurrencyUnitGenerator.satoshis,
    CurrencyUnitGenerator.satoshis) { (num1: Satoshis, num2: Satoshis) =>
    val result: Try[Int64] = Try(Int64(num1.toBigInt - num2.toBigInt))
    if (result.isSuccess && result.get >= Int64(Satoshis.min.toLong) &&
        result.get <= Int64(Satoshis.max.toLong))
      num1 - num2 == Satoshis(result.get)
    else Try(num1 - num2).isFailure
  }

  property("Multiply satoshis by zero") =
    Prop.forAll(CurrencyUnitGenerator.satoshis) { satoshis =>
      satoshis * CurrencyUnits.zero == CurrencyUnits.zero
    }

  property("Multiplicative identity") =
    Prop.forAll(CurrencyUnitGenerator.satoshis) { satoshis =>
      satoshis * Satoshis.one == satoshis

    }

  property("Multiply two satoshi values") = Prop.forAll(
    CurrencyUnitGenerator.satoshis,
    CurrencyUnitGenerator.satoshis) { (num1: Satoshis, num2: Satoshis) =>
    val result: Try[Int64] = Try(Int64(num1.toBigInt * num2.toBigInt))
    if (result.isSuccess && result.get >= Int64(Satoshis.min.toLong) &&
        result.get <= Int64(Satoshis.max.toLong))
      num1 * num2 == Satoshis(result.get)
    else Try(num1 * num2).isFailure
  }

  property("Multiply a satoshi value with an int") = Prop.forAll(
    satoshiWithInt
  ) {
    case (sat, int) =>
      val safeProduct = sat.multiplySafe(int)
      val underlyingProduct = sat.toBigInt * int
      if (underlyingProduct < Satoshis.max.toBigInt && underlyingProduct > Satoshis.min.toBigInt) {
        assert(safeProduct.isSuccess)
        safeProduct.get.satoshis.toBigInt == underlyingProduct

      } else {
        safeProduct.isFailure
      }
  }

  property("< & >=") = Prop.forAll(CurrencyUnitGenerator.satoshis,
                                   CurrencyUnitGenerator.satoshis) {
    (num1: Satoshis, num2: Satoshis) =>
      (num1 < num2) || (num1 >= num2)
  }

  property("<= & >") = Prop.forAll(CurrencyUnitGenerator.satoshis,
                                   CurrencyUnitGenerator.satoshis) {
    (num1: Satoshis, num2: Satoshis) =>
      (num1 <= num2) || (num1 > num2)
  }

  property("== & !=") = Prop.forAll(CurrencyUnitGenerator.satoshis,
                                    CurrencyUnitGenerator.satoshis) {
    (num1: Satoshis, num2: Satoshis) =>
      (num1 == num2) || (num1 != num2)
  }

  property("convert satoshis to bitcoin and then back to satoshis") = {
    Prop.forAll(CurrencyUnitGenerator.satoshis) { satoshis: Satoshis =>
      val b = Bitcoins(satoshis)
      b.satoshis == satoshis
    }
  }

  property("be able to add two unique currency unit types") = {
    Prop.forAll(CurrencyUnitGenerator.satoshis, CurrencyUnitGenerator.bitcoins) {
      (num1: Satoshis, num2: Bitcoins) =>
        val result =
          Try(Satoshis(Int64(num1.toBigInt + num2.satoshis.toBigInt)))
        val expected = result.map(Bitcoins(_))
        val actual: Try[CurrencyUnit] = Try(num1 + num2)
        if (actual.isSuccess && expected.isSuccess) actual.get == expected.get
        else actual.isFailure && expected.isFailure
    }
  }
}
