package org.bitcoins.core.gen

import org.bitcoins.core.currency.{Bitcoins, CurrencyUnit, Satoshis}
import org.scalacheck.Gen

/**
  * Created by chris on 6/23/16.
  */
trait CurrencyUnitGenerator {

  def satoshis : Gen[Satoshis] = for {
    int64 <- NumberGenerator.int64s
  } yield Satoshis(int64)

  def bitcoins: Gen[Bitcoins] = for {
    sat <- satoshis
  } yield Bitcoins(sat)

  def currencyUnit: Gen[CurrencyUnit] = Gen.oneOf(satoshis,bitcoins)

}


object CurrencyUnitGenerator extends CurrencyUnitGenerator
