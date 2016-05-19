package org.bitcoins.core.consensus

import org.bitcoins.core.currency.{Bitcoins, CurrencyUnit}

/**
  * Created by chris on 5/13/16.
  */
trait Consensus {

  def maxBlockSize = 1000000

  def maxMoney : CurrencyUnit = Bitcoins(21000000)
}

object Consensus extends Consensus
