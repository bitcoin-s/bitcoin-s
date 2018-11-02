package org.bitcoins.core.gen.ln

import org.bitcoins.core.protocol.ln._
import org.bitcoins.core.protocol.ln.currency._
import org.scalacheck.Gen

trait LnCurrencyUnitGen {

  def milliBitcoin: Gen[MilliBitcoins] = for {
    amount <- Gen.choose(MilliBitcoins.min.toLong, MilliBitcoins.max.toLong)
  } yield MilliBitcoins(amount)

  def microBitcoin: Gen[MicroBitcoins] = for {
    amount <- Gen.choose(MicroBitcoins.min.toLong, MicroBitcoins.max.toLong)
  } yield MicroBitcoins(amount)

  def nanoBitcoin: Gen[NanoBitcoins] = for {
    amount <- Gen.choose(NanoBitcoins.min.toLong, NanoBitcoins.max.toLong)
  } yield NanoBitcoins(amount)

  def picoBitcoin: Gen[PicoBitcoins] = for {
    amount <- Gen.choose(PicoBitcoins.min.toLong, PicoBitcoins.max.toLong)
  } yield PicoBitcoins(amount)

  def lnCurrencyUnit: Gen[LnCurrencyUnit] = Gen.oneOf(milliBitcoin, microBitcoin, nanoBitcoin, picoBitcoin)

  def lnCurrencyUnitOpt: Gen[Option[LnCurrencyUnit]] = {
    Gen.option(lnCurrencyUnit)
  }

  def positiveLnCurrencyUnit: Gen[LnCurrencyUnit] = {
    lnCurrencyUnit.suchThat(_ >= LnCurrencyUnits.zero)
  }

  def realisticLnInvoice: Gen[LnCurrencyUnit] = {
    positiveLnCurrencyUnit.suchThat(_ <= LnPolicy.maxAmountMSat)
  }

  def negativeLnCurrencyUnit: Gen[LnCurrencyUnit] = {
    lnCurrencyUnit.suchThat(_ < LnCurrencyUnits.zero)
  }
}

object LnCurrencyUnitGen extends LnCurrencyUnitGen