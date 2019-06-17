package org.bitcoins.core.gcs

import org.bitcoins.core.protocol.transaction.{
  TransactionOutPoint,
  TransactionOutput
}

import scala.collection.mutable

/**
  * A temporary abstraction over anything that keeps track of the UTXO set
  */
abstract class TempUtxoProvider {

  def getUtxo(
      transactionOutPoint: TransactionOutPoint): Option[TransactionOutput]
}

class TempUtxoProviderImpl extends TempUtxoProvider {

  val utxos: mutable.Map[TransactionOutPoint, TransactionOutput] =
    mutable.Map.empty

  override def getUtxo(
      transactionOutPoint: TransactionOutPoint): Option[TransactionOutput] = {
    utxos.get(transactionOutPoint)
  }
}
