package org.bitcoins.core.api.wallet

import org.bitcoins.core.api.wallet.db.AccountDb
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutput}
import org.bitcoins.core.wallet.builder.{
  FundRawTxHelper,
  ShufflingNonInteractiveFinalizer
}
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.utxo.AddressTag

import scala.concurrent.Future

trait FundTransactionHandlingApi {
  def fundRawTransaction(
      destinations: Vector[TransactionOutput],
      feeRate: FeeUnit,
      fromAccount: AccountDb,
      markAsReserved: Boolean)
      : Future[FundRawTxHelper[ShufflingNonInteractiveFinalizer]]

  /** Funds a transaction from the wallet.
    * @return
    *   funded transaction send funds to desinations with the given fee rate
    */
  def fundRawTransaction(
      destinations: Vector[TransactionOutput],
      feeRate: FeeUnit,
      fromTagOpt: Option[AddressTag],
      markAsReserved: Boolean)
      : Future[FundRawTxHelper[ShufflingNonInteractiveFinalizer]]

  def makeOpReturnCommitment(
      message: String,
      hashMessage: Boolean,
      feeRate: FeeUnit,
      fromAccount: AccountDb
  ): Future[Transaction]

  def makeOpReturnCommitment(
      message: String,
      hashMessage: Boolean,
      feeRateOpt: Option[FeeUnit],
      fromAccount: AccountDb): Future[Transaction]

  def makeOpReturnCommitment(
      message: String,
      hashMessage: Boolean,
      feeRate: FeeUnit): Future[Transaction]

  def makeOpReturnCommitment(
      message: String,
      hashMessage: Boolean,
      feeRateOpt: Option[FeeUnit]): Future[Transaction]

}
