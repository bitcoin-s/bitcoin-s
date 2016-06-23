package org.bitcoins.core.currency

import org.bitcoins.core.number.{Int64, NumberGenerator}
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
