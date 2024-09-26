package org.bitcoins.core.api.wallet

import org.bitcoins.core.api.wallet.db.{SpendingInfoDb, TransactionDb}
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.transaction.{OutputWithIndex, Transaction}
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.utxo.AddressTag
import org.bitcoins.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}

import scala.concurrent.{ExecutionContext, Future}

trait TransactionProcessingApi {
  def findByTxIds(
      txIds: Vector[DoubleSha256DigestBE]): Future[Vector[TransactionDb]]
  final def findByTxId(txId: DoubleSha256DigestBE)(implicit
      ec: ExecutionContext): Future[Option[TransactionDb]] = {
    findByTxIds(Vector(txId)).map(_.headOption)
  }
  final def findByTxId(txId: DoubleSha256Digest)(implicit
      ec: ExecutionContext): Future[Option[TransactionDb]] = {
    findByTxId(txId.flip)
  }

  def listTransactions(): Future[Vector[TransactionDb]]

  def processTransaction(
      transaction: Transaction,
      blockHashOpt: Option[DoubleSha256DigestBE]
  ): Future[Unit]

  /** Processes TXs originating from our wallet. This is called right after
    * we've signed a TX, updating our UTXO state.
    */
  def processOurTransaction(
      transaction: Transaction,
      feeRate: FeeUnit,
      inputAmount: CurrencyUnit,
      sentAmount: CurrencyUnit,
      blockHashOpt: Option[DoubleSha256DigestBE],
      newTags: Vector[AddressTag]
  ): Future[ProcessTxResult]

  def processBlock(block: Block): Future[Unit]
  def findTransaction(
      txId: DoubleSha256DigestBE
  ): Future[Option[TransactionDb]]

  def subscribeForBlockProcessingCompletionSignal(
      blockHash: DoubleSha256DigestBE
  ): Future[DoubleSha256DigestBE]

  def processReceivedUtxos(
      tx: Transaction,
      blockHashOpt: Option[DoubleSha256DigestBE],
      spendingInfoDbs: Vector[SpendingInfoDb],
      newTags: Vector[AddressTag],
      relevantReceivedOutputs: Vector[OutputWithIndex]
  ): Future[Vector[SpendingInfoDb]]

  def processSpentUtxos(
      transaction: Transaction,
      outputsBeingSpent: Vector[SpendingInfoDb],
      blockHashOpt: Option[DoubleSha256DigestBE]
  ): Future[Vector[SpendingInfoDb]]

  def insertTransaction(
      tx: Transaction,
      blockHashOpt: Option[DoubleSha256DigestBE]
  ): Future[TransactionDb]
}
