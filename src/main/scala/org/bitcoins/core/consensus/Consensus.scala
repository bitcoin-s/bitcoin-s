package org.bitcoins.core.consensus

import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.number.Int64

/**
  * Created by chris on 5/13/16.
  */
trait Consensus {

  def maxBlockSize = 1000000

  def maxMoney : CurrencyUnit = Satoshis(Int64(2100000000000000L))
}

object Consensus extends Consensus
