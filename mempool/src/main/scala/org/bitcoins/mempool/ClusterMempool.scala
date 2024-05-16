package org.bitcoins.mempool

import org.bitcoins.core.api.mempool.{MempoolAcceptResult, MempoolApi}
import org.bitcoins.core.currency.{CurrencyUnits, Satoshis}
import org.bitcoins.core.protocol.transaction.{
  Transaction,
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.mempool.config.MempoolAppConfig

import scala.collection.mutable
import scala.concurrent.Future

case class ClusterMempool(
    txoutset: Map[TransactionOutPoint, TransactionOutput])(implicit
    mempoolAppConfig: MempoolAppConfig)
    extends MempoolApi {
  locally {
    val _ = mempoolAppConfig.moduleName
  }

  private val mempool: mutable.Map[DoubleSha256DigestBE, Transaction] =
    mutable.Map[DoubleSha256DigestBE, Transaction]()
  override def acceptToMemoryPool(
      tx: Transaction): Future[MempoolAcceptResult] = {
    val result = if (!hasAllOutpoints(tx)) {
      // cannot accurately compute fees since some utxos spent
      // do not exist
      MempoolAcceptResult.Invalid(tx.vsize, Satoshis.zero)
    } else {
      mempool.addOne((tx.txIdBE, tx))
      val result = MempoolAcceptResult.Valid(
        tx.vsize,
        ClusterMempool.getTotalFees(txoutset, tx))
      result
    }
    Future.successful(result)
  }

  private def hasAllOutpoints(tx: Transaction): Boolean = {
    tx.inputs.forall(i => txoutset.get(i.previousOutput).isDefined)
  }
}

object ClusterMempool {
  def getTotalFees(
      txoutset: Map[TransactionOutPoint, TransactionOutput],
      tx: Transaction): Satoshis = {
    val fundingAmount = tx.inputs
      .foldLeft(CurrencyUnits.zero) { case (accum, input) =>
        txoutset.get(input.previousOutput) match {
          case Some(output) =>
            accum + output.value.satoshis
          case None =>
            sys.error(
              s"Cannot find output corresponding to outpoint=${input.previousOutput} in tx=${tx.txIdBE}")
        }
      }

    val spendingAmount = tx.outputs.map(_.value).sum

    (fundingAmount - spendingAmount).satoshis
  }
}
