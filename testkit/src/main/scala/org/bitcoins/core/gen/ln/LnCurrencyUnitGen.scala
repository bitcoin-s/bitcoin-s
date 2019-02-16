package org.bitcoins.core.gen.ln

import org.bitcoins.core.gen.NumberGenerator
import org.bitcoins.core.protocol.ln._
import org.bitcoins.core.protocol.ln.currency._
import org.scalacheck.Gen

trait LnCurrencyUnitGen {

  def milliBitcoin: Gen[MilliBitcoins] =
    for {
      amount <- Gen.choose(MilliBitcoins.min.toLong, MilliBitcoins.max.toLong)
    } yield MilliBitcoins(amount)

  def microBitcoin: Gen[MicroBitcoins] =
    for {
      amount <- Gen.choose(MicroBitcoins.min.toLong, MicroBitcoins.max.toLong)
    } yield MicroBitcoins(amount)

  def nanoBitcoin: Gen[NanoBitcoins] =
    for {
      amount <- Gen.choose(NanoBitcoins.min.toLong, NanoBitcoins.max.toLong)
    } yield NanoBitcoins(amount)

  def picoBitcoin: Gen[PicoBitcoins] =
    for {
      amount <- Gen.choose(PicoBitcoins.min.toLong, PicoBitcoins.max.toLong)
    } yield PicoBitcoins(amount)

  def positivePicoBitcoin: Gen[PicoBitcoins] = {
    Gen.choose(0, PicoBitcoins.max.toLong).map(PicoBitcoins(_))
  }

  def lnCurrencyUnit: Gen[LnCurrencyUnit] =
    Gen.oneOf(milliBitcoin, microBitcoin, nanoBitcoin, picoBitcoin)

  def lnCurrencyUnitOpt: Gen[Option[LnCurrencyUnit]] = {
    Gen.option(lnCurrencyUnit)
  }

  def positiveLnCurrencyUnit: Gen[LnCurrencyUnit] = {
    lnCurrencyUnit.suchThat(_ >= LnCurrencyUnits.zero)
  }

  def realisticLnInvoice: Gen[LnCurrencyUnit] = {
    positiveLnCurrencyUnit.suchThat(_.toMSat <= LnPolicy.maxAmountMSat)
  }

  def negativeLnCurrencyUnit: Gen[LnCurrencyUnit] = {
    lnCurrencyUnit.suchThat(_ < LnCurrencyUnits.zero)
  }

  def milliSatoshis: Gen[MilliSatoshis] =
    for {
      i64 <- NumberGenerator.uInt64
    } yield MilliSatoshis(i64.toBigInt)

  def milliSatoshisPair: Gen[(MilliSatoshis, MilliSatoshis)] =
    for {
      first <- milliSatoshis
      second <- milliSatoshis
    } yield (first, second)
}

object LnCurrencyUnitGen extends LnCurrencyUnitGen
