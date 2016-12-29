package org.bitcoins.core.consensus

import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.number.Int64
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.transaction.{BaseTransaction, WitnessTransaction}
import org.bitcoins.core.serializers.transaction.RawBaseTransactionParser

/**
  * Created by chris on 5/13/16.
  */
trait Consensus {

  def maxBlockSize = 1000000

  def maxBlockWeight = maxBlockSize * 4

  /** This is the new computation to determine the maximum size of a block as per BIP141
    * [[https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki#block-size]]
    * The weight of a block is determined as follows:
    *
    * Base size is the block size in bytes with the original transaction serialization without any witness-related data
    *
    * Total size is the block size in bytes with transactions serialized as described in BIP144, including base data and witness data.
    *
    * Block weight is defined as Base size * 3 + Total size. (rationale[3])
    */
  def blockWeight(block: Block): Long = {
    val (baseSize,totalSize) = block.transactions.map {
      case btx: BaseTransaction => (btx.size,btx.size)
      case wtx: WitnessTransaction =>
        val btx = BaseTransaction(wtx.version,wtx.inputs,wtx.outputs,wtx.lockTime)
        (btx.size,wtx.size)
    }.fold((0,0)) {
      case ((baseAccum,totalAccum),(base,total)) => (baseAccum + base, totalAccum + total)
    }

    val weight = baseSize * 3 + totalSize
    weight
  }
  /**
    * BIP141 changes this from 20,000 -> 80,000, to see how sigops are counted please see BIP 141
    * [[https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki#sigops]]
    */
  def maxSigOps = 80000

  def maxMoney : CurrencyUnit = Satoshis(Int64(2100000000000000L))
}

object Consensus extends Consensus
