package org.bitcoins.core.currency

import org.bitcoins.core.number.Int64
import org.bitcoins.testkit.core.gen.CurrencyUnitGenerator
import org.bitcoins.testkit.util.BitcoinSUnitTest
import org.scalacheck.Gen

import scala.util.Try

class CurrencyUnitTest extends BitcoinSUnitTest {

  behavior of "Satoshis"

  it must "have 1BTC == 100,000,000 satoshis" in {
    assert(Bitcoins.one == Satoshis(100000000))
    assert(Satoshis(100000000) == Bitcoins.one)
    assert(Satoshis.zero == Bitcoins.zero)
    assert(Bitcoins.zero == Satoshis.zero)
  }

  it must "have symmetry serialization" in {
    forAll(CurrencyUnitGenerator.satoshis) { satoshis =>
      assert(Satoshis(satoshis.hex) == satoshis)
    }
  }

  it must "have Int syntax" in {
    forAll(Gen.choose(0, Int.MaxValue)) { num =>
      assert(num.bitcoins == Bitcoins(num))
      assert(num.bitcoin == Bitcoins(num))
      assert(num.BTC == Bitcoins(num))

      assert(num.satoshis == Satoshis(num))
      assert(num.satoshi == Satoshis(num))
      assert(num.sats == Satoshis(num))
      assert(num.sat == Satoshis(num))
    }
  }

  it must "have Long syntax" in {
    forAll(Gen.choose(0, Satoshis.max.toLong / 100000000)) { num =>
      assert(num.bitcoins == Bitcoins(num))
      assert(num.bitcoin == Bitcoins(num))
      assert(num.BTC == Bitcoins(num))
    }

    forAll(Gen.choose(0, Satoshis.max.toLong)) { num =>
      assert(num.satoshis == Satoshis(num))
      assert(num.satoshi == Satoshis(num))
      assert(num.sats == Satoshis(num))
      assert(num.sat == Satoshis(num))
    }
  }

  it must "have additive identity" in {
    forAll(CurrencyUnitGenerator.satoshis) { satoshis =>
      assert(satoshis + CurrencyUnits.zero == satoshis)
    }
  }

  it must "add satoshis" in {
    forAll(CurrencyUnitGenerator.satoshis, CurrencyUnitGenerator.satoshis) {
      (_, _) => (num1: Satoshis, num2: Satoshis) =>
        val result: Try[Int64] = Try(Int64(num1.toBigInt + num2.toBigInt))
        if (
          result.isSuccess && result.get >= Int64(Satoshis.min.toLong) &&
          result.get <= Int64(Satoshis.max.toLong)
        )
          assert(num1 + num2 == Satoshis(result.get))
        else assert(Try(num1 + num2).isFailure)
    }
  }

  it must "have subtractive identity for satoshis" in {
    forAll(CurrencyUnitGenerator.satoshis) { satoshis =>
      assert(satoshis - CurrencyUnits.zero == satoshis)
    }
  }

  it must "subtract satoshis" in {
    forAll(CurrencyUnitGenerator.satoshis, CurrencyUnitGenerator.satoshis) {
      (num1: Satoshis, num2: Satoshis) =>
        val result: Try[Int64] = Try(Int64(num1.toBigInt - num2.toBigInt))
        if (
          result.isSuccess && result.get >= Int64(Satoshis.min.toLong) &&
          result.get <= Int64(Satoshis.max.toLong)
        )
          assert(num1 - num2 == Satoshis(result.get))
        else assert(Try(num1 - num2).isFailure)
    }
  }

  it must "multiply satoshis by zero" in {
    forAll(CurrencyUnitGenerator.satoshis) { satoshis =>
      assert(satoshis * CurrencyUnits.zero == CurrencyUnits.zero)
    }
  }

  it must "have multiplicative identity" in {
    forAll(CurrencyUnitGenerator.satoshis) { satoshis =>
      assert(satoshis * Satoshis.one == satoshis)
    }
  }

  it must "multiply satoshis" in {
    forAll(CurrencyUnitGenerator.satoshis, CurrencyUnitGenerator.satoshis) {
      (num1, num2) =>
        val result: Try[Int64] = Try(Int64(num1.toBigInt * num2.toBigInt))
        if (
          result.isSuccess && result.get >= Int64(Satoshis.min.toLong) &&
          result.get <= Int64(Satoshis.max.toLong)
        )
          num1 * num2 == Satoshis(result.get)
        else Try(num1 * num2).isFailure
    }
  }

  private val satoshiWithInt: Gen[(Satoshis, Int)] = for {
    sat <- CurrencyUnitGenerator.satoshis
    num <- Gen.choose(Int.MinValue, Int.MaxValue)
  } yield (sat, num)

  it must "multiply a satoshi value with an int" in {
    forAll(satoshiWithInt) {
      case (sat, int) =>
        val safeProduct = sat.multiplySafe(int)
        val underlyingProduct = sat.toBigInt * int
        if (
          underlyingProduct < Satoshis.max.toBigInt && underlyingProduct > Satoshis.min.toBigInt
        ) {
          assert(safeProduct.isSuccess)
          assert(safeProduct.get.satoshis.toBigInt == underlyingProduct)
        } else {
          assert(safeProduct.isFailure)
        }
    }
  }

  it must "have '< & >=' property" in {
    forAll(CurrencyUnitGenerator.satoshis, CurrencyUnitGenerator.satoshis) {
      (num1, num2) =>
        assert((num1 < num2) || (num1 >= num2))
    }
  }

  it must "have '<= & >' property" in {
    forAll(CurrencyUnitGenerator.satoshis, CurrencyUnitGenerator.satoshis) {
      (num1, num2) =>
        assert((num1 <= num2) || (num1 > num2))
    }
  }

  it must "have '== & !=' property" in {
    forAll(CurrencyUnitGenerator.satoshis, CurrencyUnitGenerator.satoshis) {
      (num1, num2) =>
        assert((num1 == num2) || (num1 != num2))
    }
  }

  it must "convert satoshis to bitcoin and then back to satoshis" in {
    forAll(CurrencyUnitGenerator.satoshis) { satoshis =>
      val b = Bitcoins(satoshis)
      assert(b.satoshis == satoshis)
      assert(CurrencyUnits.toSatoshis(b) == satoshis)
    }
  }

  it must "be able to add two unique currency unit types" in {
    forAll(CurrencyUnitGenerator.satoshis, CurrencyUnitGenerator.bitcoins) {
      (sats: Satoshis, btc: Bitcoins) =>
        val result =
          Try(Satoshis(sats.toBigInt + btc.satoshis.toBigInt))
        val expected = result.map(Bitcoins(_))
        val actual: Try[CurrencyUnit] = Try(sats + btc)
        if (actual.isSuccess && expected.isSuccess) actual.get == expected.get
        else actual.isFailure && expected.isFailure
    }
  }

  it must "correctly compare two currency units" in {
    forAll(CurrencyUnitGenerator.satoshis, CurrencyUnitGenerator.satoshis) {
      (num1, num2) =>
        if (num1 > num2)
          assert(num1.compare(num2) == 1)
        else if (num1 < num2)
          assert(num1.compare(num2) == -1)
        else
          assert(num1.compare(num2) == 0)
    }
  }

  private val smallSatGen = Gen.choose(0, 10000).map { n =>
    Satoshis(n)
  }

  it must "have correct/consistent Numeric functions" in {
    forAll(smallSatGen, smallSatGen) {
      case (num1, num2) =>
        assert(num1 + num2 == CurrencyUnits.plus(num1, num2))
        assert(num1 - num2 == CurrencyUnits.minus(num1, num2))
        assert(num1 * num2 == CurrencyUnits.times(num1, num2))
        assert(-num1 == CurrencyUnits.negate(num1))
        assert(CurrencyUnits.fromInt(CurrencyUnits.toInt(num1)) == num1)
        assert(CurrencyUnits.fromInt(CurrencyUnits.toLong(num1).toInt) == num1)
        assert(CurrencyUnits.fromInt(CurrencyUnits.toFloat(num1).toInt) == num1)
        assert(
          CurrencyUnits.fromInt(CurrencyUnits.toDouble(num1).toInt) == num1)

        assert(CurrencyUnits.parseString("").isEmpty)
        val str = "Never gonna give you up, never gonna let you down"
        assert(CurrencyUnits.parseString(str).isEmpty)
        assert(
          CurrencyUnits
            .parseString("12345678900")
            .contains(Satoshis(12345678900L)))
    }
  }

  it must "convert to and from BigDemimals correctly" in {
    forAll(CurrencyUnitGenerator.bitcoins) { num =>
      assert(Bitcoins(num.toBigDecimal) == num)
    }
  }

  it must "convert to and from hex correctly" in {
    forAll(CurrencyUnitGenerator.bitcoins) { num =>
      assert(Satoshis.fromHex(num.hex) == num)
    }
  }
}
