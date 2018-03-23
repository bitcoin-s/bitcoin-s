package org.bitcoins.core.consensus

import org.bitcoins.core.currency.{ CurrencyUnit, Satoshis }
import org.bitcoins.core.number.Int64
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.transaction.{ BaseTransaction, WitnessTransaction }
import org.bitcoins.core.serializers.transaction.RawBaseTransactionParser

/**
 * Created by chris on 5/13/16.
 */
sealed abstract class Consensus {

  def maxBlockSize: Long = 1000000

  def weightScalar: Long = 4

  def maxBlockWeight: Long = maxBlockSize * weightScalar

  /**
   * BIP141 changes this from 20,000 -> 80,000, to see how sigops are counted please see BIP 141
   * [[https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki#sigops]]
   */
  def maxSigOps = 80000

  def maxMoney: CurrencyUnit = Satoshis(Int64(2100000000000000L))
}

object Consensus extends Consensus
