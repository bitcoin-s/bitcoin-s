package org.bitcoins.core.protocol.ln.currency

import org.bitcoins.testkitcore.gen.ln.LnCurrencyUnitGen
import org.bitcoins.testkitcore.gen.{CurrencyUnitGenerator, NumberGenerator}
import org.bitcoins.testkitcore.util.BitcoinSUnitTest
import org.scalacheck.Gen

class MilliSatoshisTest extends BitcoinSUnitTest {
  behavior of "MilliSatoshis"

  it must "convert pico bitcoins to msat correctly" in {

    MilliSatoshis.fromPico(PicoBitcoins.zero) must be(MilliSatoshis.zero)
    MilliSatoshis.fromPico(PicoBitcoins.one) must be(MilliSatoshis.zero)
    MilliSatoshis.fromPico(PicoBitcoins(9)) must be(MilliSatoshis.zero)

    MilliSatoshis.fromPico(PicoBitcoins(10)) must be(MilliSatoshis.one)

    MilliSatoshis.fromPico(PicoBitcoins(19)) must be(MilliSatoshis.one)

    MilliSatoshis.fromPico(PicoBitcoins(20)) must be(MilliSatoshis(2))

    MilliSatoshis.fromPico(PicoBitcoins(101)) must be(MilliSatoshis(10))

    MilliSatoshis.fromPico(PicoBitcoins(110)) must be(MilliSatoshis(11))
  }

  it must "add millisatoshis" in {
    forAll(LnCurrencyUnitGen.milliSatoshisPair) { case (first, second) =>
      val bigInt = first.toBigInt + second.toBigInt
      assert((first + second).toBigInt == bigInt)
    }
  }

  private val msatWithNum = for {
    msat <- LnCurrencyUnitGen.milliSatoshis
    num <- NumberGenerator.bigIntsUInt64Range.filter(_ > 0)
  } yield (msat, num)

  it must "multiply millisatoshis with an int" in {
    forAll(msatWithNum) { case (msat, bigint) =>
      val underlyingCalc = msat.toBigInt * bigint
      assert((msat * bigint).toBigInt == underlyingCalc)
    }
  }

  it must "multiply millisatoshis with itself" in {
    forAll(LnCurrencyUnitGen.milliSatoshisPair) { case (first, second) =>
      val safe = first.multiplySafe(second)
      val unsafe = first * second

      assert(safe.toOption.contains(unsafe))

      val underlying = first.toBigInt * second.toBigInt
      assert(unsafe.toBigInt == underlying)
    }
  }

  it must "subtract msats after adding them" in {
    forAll(LnCurrencyUnitGen.milliSatoshisPair) { case (first, second) =>
      val added = first + second
      val subtracted = added - second
      assert(subtracted == first)
    }
  }

  it must "subtract msats" in {
    forAll(LnCurrencyUnitGen.milliSatoshisPair) { case (first, second) =>
      val subtracted = first subtractSafe second
      val isPositive = (first.toBigInt - second.toBigInt) >= 0

      assert(subtracted.isSuccess == isPositive)
      if (subtracted.isSuccess) {
        val underlyingCalc = first.toBigInt - second.toBigInt
        assert(subtracted.get.toBigInt == underlyingCalc)
      }
    }
  }

  it must "covert from a ln currency unit -> millisatoshis -> lnCurrencyUnit" in {
    forAll(LnCurrencyUnitGen.positivePicoBitcoin) { pb =>
      val underlying = pb.toBigInt
      //we lose the last digit of precision converting
      //PicoBitcoins -> MilliSatoshis
      //this is the expected answer
      val expected = (underlying / 10) * 10
      val expectedPico = PicoBitcoins(expected)

      val pico = PicoBitcoins(underlying)

      val msat = MilliSatoshis(pico)

      val lnCurrencyUnit = msat.toLnCurrencyUnit

      assert(expectedPico == lnCurrencyUnit)
    }
  }

  it must "convert sat -> msat -> sat" in {
    forAll(CurrencyUnitGenerator.positiveRealistic) { sat =>
      val msat = MilliSatoshis(sat)
      assert(msat.toSatoshis == sat)

    }
  }

  it must "have Int syntax" in {
    forAll(Gen.choose(0, Int.MaxValue)) { num =>
      assert(num.millisatoshis == MilliSatoshis(num))
      assert(num.millisatoshi == MilliSatoshis(num))
      assert(num.msats == MilliSatoshis(num))
      assert(num.msat == MilliSatoshis(num))
    }
  }

  it must "have Long syntax" in {
    forAll(Gen.choose(0L, Long.MaxValue)) { num =>
      assert(num.millisatoshis == MilliSatoshis(num))
      assert(num.millisatoshi == MilliSatoshis(num))
      assert(num.msats == MilliSatoshis(num))
      assert(num.msat == MilliSatoshis(num))
    }
  }

  it must "toString msat and msats correctly" in {
    assert(MilliSatoshis(1).toString == "1 msat")
    assert(MilliSatoshis(2).toString == "2 msats")
  }

  it must "convert Millisatoshis toBigDecimal and back" in {
    forAll(LnCurrencyUnitGen.milliSatoshis) { milliSats =>
      assert(milliSats.toBigDecimal.toBigIntExact.contains(milliSats.toBigInt))
    }
  }

  it must "compare Millisatoshis correctly" in {
    forAll(LnCurrencyUnitGen.milliSatoshis) { msat =>
      val msatPlusOne = msat + MilliSatoshis.one

      assert(msat == msat)
      assert(msat != msatPlusOne)
      assert(msat < msatPlusOne)
      assert(msat <= msatPlusOne)
      assert(msat <= msat)
      assert(msatPlusOne > msat)
      assert(msatPlusOne >= msat)
      assert(msat >= msat)
    }
  }

  it must "convert to and from UInt64 correctly" in {
    forAll(LnCurrencyUnitGen.milliSatoshis) { msat =>
      assert(MilliSatoshis(msat.toUInt64.toBigInt) == msat)
    }
  }
}
