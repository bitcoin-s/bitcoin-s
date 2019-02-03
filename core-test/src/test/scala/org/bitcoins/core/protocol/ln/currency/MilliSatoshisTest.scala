package org.bitcoins.core.protocol.ln.currency

import org.bitcoins.core.gen.CurrencyUnitGenerator
import org.bitcoins.core.gen.ln.LnCurrencyUnitGen
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, MustMatchers}
import org.slf4j.LoggerFactory

class MilliSatoshisTest extends FlatSpec with MustMatchers {
  private val logger = LoggerFactory.getLogger(this.getClass)
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

  it must "covert from a ln currency unit -> millisatoshis -> lnCurrencyUnit" in {

    PropertyChecks.forAll(LnCurrencyUnitGen.positivePicoBitcoin) { pb =>
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
    PropertyChecks.forAll(CurrencyUnitGenerator.positiveRealistic) { sat =>
      val msat = MilliSatoshis(sat)
      assert(msat.toSatoshis == sat)

    }
  }
}
