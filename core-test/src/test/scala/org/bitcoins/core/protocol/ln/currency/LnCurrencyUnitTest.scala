package org.bitcoins.core.protocol.ln.currency

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.ln.LnPolicy
import org.bitcoins.testkit.core.gen.ln.LnCurrencyUnitGen
import org.bitcoins.testkit.util.BitcoinSUnitTest
import org.scalacheck.Gen

import scala.util.{Failure, Success, Try}

class LnCurrencyUnitTest extends BitcoinSUnitTest {

  it must "have additive identity" in {
    forAll(LnCurrencyUnitGen.lnCurrencyUnit) { lnUnit =>
      assert(lnUnit + LnCurrencyUnits.zero == lnUnit)
    }
  }

  it must "add to LnCurrencyUnits" in {
    forAll(LnCurrencyUnitGen.lnCurrencyUnit, LnCurrencyUnitGen.lnCurrencyUnit) {
      (num1, num2) =>
        val resultT: Try[LnCurrencyUnit] = Try(num1 + num2)
        resultT match {
          case Success(result) =>
            assert(PicoBitcoins.min <= result)
            assert(result <= PicoBitcoins.max)
            assert(num1 + num2 == result)
          case Failure(_) => succeed
        }
    }
  }

  it must "have subtractive identity for LnCurrencyUnits" in {
    forAll(LnCurrencyUnitGen.lnCurrencyUnit) { lnUnit =>
      assert(lnUnit - LnCurrencyUnits.zero == lnUnit)
    }
  }

  it must "subtract two LnCurrencyUnit values" in {
    forAll(LnCurrencyUnitGen.lnCurrencyUnit, LnCurrencyUnitGen.lnCurrencyUnit) {
      (num1, num2) =>
        val resultT: Try[LnCurrencyUnit] = Try(num1 - num2)
        resultT match {
          case Success(result) =>
            assert(PicoBitcoins.min <= result)
            assert(result <= PicoBitcoins.max)
            assert(num1 - num2 == result)
          case Failure(_) => succeed
        }
    }
  }

  it must "multiply LnCurrencyUnit by zero" in {
    forAll(LnCurrencyUnitGen.lnCurrencyUnit) { lnUnit =>
      assert(lnUnit * LnCurrencyUnits.zero == LnCurrencyUnits.zero)
    }
  }

  it must "have multiplicative identity for LnCurrencyUnits" in {
    forAll(LnCurrencyUnitGen.lnCurrencyUnit) { lnUnit =>
      assert(lnUnit * PicoBitcoins.one == lnUnit)
    }
  }

  it must "multiply two LnCurrencyUnit values" in {
    forAll(LnCurrencyUnitGen.lnCurrencyUnit, LnCurrencyUnitGen.lnCurrencyUnit) {
      (num1, num2) =>
        val resultT: Try[LnCurrencyUnit] = Try(num1 * num2)
        resultT match {
          case Success(result) =>
            assert(result >= PicoBitcoins.min)
            assert(result <= PicoBitcoins.max)
            assert(num1 * num2 == result)
          case Failure(_) => succeed
        }
    }
  }

  private val lnCurrWithInt: Gen[(LnCurrencyUnit, Int)] = for {
    ln <- LnCurrencyUnitGen.lnCurrencyUnit
    num <- Gen.choose(Int.MinValue, Int.MaxValue)
  } yield (ln, num)

  it must "multiply a LnCurrencyUnit value with an int" in {
    forAll(lnCurrWithInt) {
      case (ln, int) =>
        val safeProduct = ln.multiplySafe(int)
        val underlyingProduct = ln.toBigInt * int
        if (
          underlyingProduct <= PicoBitcoins.min.toBigInt && underlyingProduct >= PicoBitcoins.max.toBigInt
        ) {
          assert(safeProduct.isSuccess)
          safeProduct.get.toBigInt == underlyingProduct
        } else {
          safeProduct.isFailure
        }
    }
  }

  it must "have property '< & >=''" in {
    forAll(LnCurrencyUnitGen.lnCurrencyUnit, LnCurrencyUnitGen.lnCurrencyUnit) {
      (num1, num2) =>
        assert((num1 < num2) || (num1 >= num2))
    }
  }

  it must "have property '<= & >'" in {
    forAll(LnCurrencyUnitGen.lnCurrencyUnit, LnCurrencyUnitGen.lnCurrencyUnit) {
      (num1, num2) =>
        assert((num1 <= num2) || (num1 > num2))
    }
  }

  it must "have property '== & !='" in {
    forAll(LnCurrencyUnitGen.lnCurrencyUnit, LnCurrencyUnitGen.lnCurrencyUnit) {
      (num1, num2) =>
        assert((num1 == num2) || (num1 != num2))
    }
  }

  it must "have Int syntax" in {
    forAll(Gen.choose(Int.MinValue, Int.MaxValue)) { num =>
      assert(num.millibitcoins == MilliBitcoins(num))
      assert(num.millibitcoin == MilliBitcoins(num))
      assert(num.mBTC == MilliBitcoins(num))

      assert(num.microbitcoins == MicroBitcoins(num))
      assert(num.microbitcoin == MicroBitcoins(num))
      assert(num.uBTC == MicroBitcoins(num))

      assert(num.nanobitcoins == NanoBitcoins(num))
      assert(num.nanobitcoin == NanoBitcoins(num))
      assert(num.nBTC == NanoBitcoins(num))

      assert(num.picobitcoins == PicoBitcoins(num))
      assert(num.picobitcoin == PicoBitcoins(num))
      assert(num.pBTC == PicoBitcoins(num))
    }
  }

  it must "have Long syntax" in {
    forAll(
      Gen.choose(LnPolicy.minMilliBitcoins.toLong,
                 LnPolicy.maxMilliBitcoins.toLong)) { num =>
      assert(num.millibitcoins == MilliBitcoins(num))
      assert(num.millibitcoin == MilliBitcoins(num))
      assert(num.mBTC == MilliBitcoins(num))

      assert(num.microbitcoins == MicroBitcoins(num))
      assert(num.microbitcoin == MicroBitcoins(num))
      assert(num.uBTC == MicroBitcoins(num))

      assert(num.nanobitcoins == NanoBitcoins(num))
      assert(num.nanobitcoin == NanoBitcoins(num))
      assert(num.nBTC == NanoBitcoins(num))

      assert(num.picobitcoins == PicoBitcoins(num))
      assert(num.picobitcoin == PicoBitcoins(num))
      assert(num.pBTC == PicoBitcoins(num))
    }
  }

  it must "serialize MilliBitcoins to string" in {
    val milliBitcoins = MilliBitcoins(1000)
    milliBitcoins.toEncodedString must be("1000m")
  }

  it must "serialize MicroBitcoins to string" in {
    val microBitcoins = MicroBitcoins(1000)
    microBitcoins.toEncodedString must be("1000u")
  }

  it must "serialize NanoBitcoins to string" in {
    val nanoBitcoins = NanoBitcoins(1000)
    nanoBitcoins.toEncodedString must be("1000n")
  }

  it must "serialize PicoBitcoins to string" in {
    val picoBitcoins = PicoBitcoins(1000)
    picoBitcoins.toEncodedString must be("1000p")
  }

  it must "deserialize MilliBitcoins from string" in {
    val input = "1000m"
    LnCurrencyUnits.fromEncodedString(input).get must be(MilliBitcoins(1000))
  }

  it must "deserialize MicroBitcoins from string" in {
    val input = "1000u"
    LnCurrencyUnits.fromEncodedString(input).get must be(MicroBitcoins(1000))
  }

  it must "deserialize NanoBitcoins from string" in {
    val input = "1000n"
    LnCurrencyUnits.fromEncodedString(input).get must be(NanoBitcoins(1000))
  }

  it must "deserialize PicoBitcoins from string" in {
    val input = "1000p"
    LnCurrencyUnits.fromEncodedString(input).get must be(PicoBitcoins(1000))
  }

  it must "fail to deserialize an invalid amount" in {
    val input = "10000000000000000m"
    LnCurrencyUnits.fromEncodedString(input).isFailure must be(true)
  }

  it must "fail to deserialize an invalid number" in {
    val input = "10z00m"
    LnCurrencyUnits.fromEncodedString(input).isFailure must be(true)
  }

  it must "fail to deserialize an invalid currency denomination" in {
    val input = "1000z"
    LnCurrencyUnits.fromEncodedString(input).isFailure must be(true)
  }

  it must "have the correct maximum and minimum number representation for MilliBitcoins" in {
    MilliBitcoins.max must be(MilliBitcoins(9223372036L))
    MilliBitcoins.min must be(MilliBitcoins(-9223372036L))
  }

  it must "have the correct maximum and minimum number representation for MicroBitcoins" in {
    MicroBitcoins.max must be(MicroBitcoins(9223372036854L))
    MicroBitcoins.min must be(MicroBitcoins(-9223372036854L))
  }

  it must "have the correct maximum and minimum number representation for NanoBitcoins" in {
    NanoBitcoins.max must be(NanoBitcoins(9223372036854775L))
    NanoBitcoins.min must be(NanoBitcoins(-9223372036854775L))
  }

  it must "have the correct maximum and minimum number representation for PicoBitcoins" in {
    PicoBitcoins.max must be(PicoBitcoins(9223372036854775807L))
    PicoBitcoins.min must be(PicoBitcoins(-9223372036854775808L))
  }

  it must "round pico bitcoins to satoshis correctly" in {
    PicoBitcoins.one.toSatoshis must be(Satoshis.zero)

    PicoBitcoins(9999).toSatoshis must be(Satoshis.zero)

    PicoBitcoins(10000).toSatoshis must be(Satoshis.one)

    PicoBitcoins(19999).toSatoshis must be(Satoshis.one)
  }

  it must "convert units to the correct pico bitcoins amount" in {

    val expectedNano = BigInt(10).pow(3)
    NanoBitcoins.one.toPicoBitcoins must be(PicoBitcoins(expectedNano))

    val expectedMicro = BigInt(10).pow(6)
    MicroBitcoins.one.toPicoBitcoins must be(PicoBitcoins(expectedMicro))

    val expectedMilli = BigInt(10).pow(9)
    MilliBitcoins.one.toPicoBitcoins must be(PicoBitcoins(expectedMilli))
  }

  it must "fail to create a MilliBitcoin outside of the maximum range" in {
    intercept[IllegalArgumentException] {
      MilliBitcoins(LnPolicy.maxMilliBitcoins + 1)
    }
  }

  it must "fail to create a MicroBitcoin outside of the maximum range" in {
    intercept[IllegalArgumentException] {
      MicroBitcoins(LnPolicy.maxMicroBitcoins + 1)
    }
  }

  it must "fail to create a NanoBitcoin outside of the maximum range" in {
    intercept[IllegalArgumentException] {
      NanoBitcoins(LnPolicy.maxNanoBitcoins + 1)
    }
  }

  it must "fail to create a PicoBitcion outside of the maximum range" in {
    intercept[IllegalArgumentException] {
      PicoBitcoins(LnPolicy.maxPicoBitcoins + 1)
    }
  }

  it must "fail to create a MilliBitcoin outside of the minimum range" in {
    intercept[IllegalArgumentException] {
      MilliBitcoins(LnPolicy.minMilliBitcoins - 1)
    }
  }

  it must "fail to create a MicroBitcoin outside of the minimum range" in {
    intercept[IllegalArgumentException] {
      MicroBitcoins(LnPolicy.minMicroBitcoins - 1)
    }
  }

  it must "fail to create a NanoBitcoin outside of the minimum range" in {
    intercept[IllegalArgumentException] {
      NanoBitcoins(LnPolicy.minNanoBitcoins - 1)
    }
  }

  it must "fail to create a PicoBitcion outside of the minimum range" in {
    intercept[IllegalArgumentException] {
      PicoBitcoins(LnPolicy.minPicoBitcoins - 1)
    }
  }

  it must "have the correct representation for 0" in {
    MilliBitcoins.zero must be(MilliBitcoins(0))
    MicroBitcoins.zero must be(MicroBitcoins(0))
    NanoBitcoins.zero must be(NanoBitcoins(0))
    PicoBitcoins.zero must be(PicoBitcoins(0))
    LnCurrencyUnits.zero must be(PicoBitcoins(0))
  }

  it must "have the correct representation for 1" in {
    MilliBitcoins.one must be(MilliBitcoins(1))
    MicroBitcoins.one must be(MicroBitcoins(1))
    NanoBitcoins.one must be(NanoBitcoins(1))
    PicoBitcoins.one must be(PicoBitcoins(1))
  }
}
