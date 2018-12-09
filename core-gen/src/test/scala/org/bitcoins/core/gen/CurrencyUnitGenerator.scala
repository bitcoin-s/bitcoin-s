package org.bitcoins.core.gen

import org.bitcoins.core.currency.{
  Bitcoins,
  CurrencyUnit,
  CurrencyUnits,
  Satoshis
}
import org.bitcoins.core.number.Int64
import org.scalacheck.Gen

/**
  * Created by chris on 6/23/16.
  */
trait CurrencyUnitGenerator {

  def satoshis: Gen[Satoshis] =
    for {
      int64 <- NumberGenerator.int64s
    } yield Satoshis(int64)

  def bitcoins: Gen[Bitcoins] =
    for {
      sat <- satoshis
    } yield Bitcoins(sat)

  def currencyUnit: Gen[CurrencyUnit] = Gen.oneOf(satoshis, bitcoins)

  def positiveSatoshis: Gen[Satoshis] =
    satoshis.suchThat(_ >= CurrencyUnits.zero)

  /**
    * Generates a postiive satoshi value that is 'realistic'. This current 'realistic' range
    * is from 0 to 1,000,000 bitcoin
    */
  def positiveRealistic: Gen[Satoshis] =
    Gen.choose(0, Bitcoins(1000000).satoshis.toLong).map { n =>
      Satoshis(Int64(n))
    }
}

object CurrencyUnitGenerator extends CurrencyUnitGenerator
