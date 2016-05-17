package org.bitcoins.consensus

import org.bitcoins.currency.{Bitcoins, CurrencyUnit}

/**
  * Created by chris on 5/13/16.
  */
trait Consensus {

  def maxBlockSize = 1000000

  def maxMoney : CurrencyUnit = Bitcoins(21000000)
}

object Consensus extends Consensus
