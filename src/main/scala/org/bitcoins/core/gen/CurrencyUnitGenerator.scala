package org.bitcoins.core.gen

import org.bitcoins.core.currency.Satoshis
import org.scalacheck.Gen

/**
  * Created by chris on 6/23/16.
  */
trait CurrencyUnitGenerator {

  def satoshis : Gen[Satoshis] = for {
    int64 <- NumberGenerator.int64s
  } yield Satoshis(int64)

}


object CurrencyUnitGenerator extends CurrencyUnitGenerator
