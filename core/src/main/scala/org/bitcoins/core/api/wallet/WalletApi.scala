package org.bitcoins.core.api.wallet

import org.bitcoins.core.api.chain.ChainQueryApi
import org.bitcoins.core.api.feeprovider.FeeRateApi
import org.bitcoins.core.api.node.NodeApi
import org.bitcoins.core.api.wallet.db.*
import org.bitcoins.core.crypto.ExtPublicKey
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.hd.HDAccount
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.{
  Transaction,
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.util.StartStopAsync
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.utxo.{AddressTag, TxoState}
import org.bitcoins.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}

import java.time.Instant
import scala.concurrent.{ExecutionContext, Future}

/** API for the wallet project.
  *
  * This wallet API is BIP44 compliant.
  *
  * @see
  *   [[https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki BIP44]]
  */
trait WalletApi extends StartStopAsync[WalletApi] {
  def accountHandling: AccountHandlingApi
  def fundTxHandling: FundTransactionHandlingApi
  def rescanHandling: RescanHandlingApi
  def addressHandling: AddressHandlingApi
  def utxoHandling: UtxoHandlingApi
  def transactionProcessing: TransactionProcessingApi
  def sendFundsHandling: SendFundsHandlingApi

  val nodeApi: NodeApi
  val chainQueryApi: ChainQueryApi
  val feeRateApi: FeeRateApi
  val creationTime: Instant

  def broadcastTransaction(transaction: Transaction): Future[Unit] =
    nodeApi.broadcastTransaction(transaction)

  def getTransactionsToBroadcast: Future[Vector[Transaction]]

  def getFeeRate(): Future[FeeUnit] = feeRateApi.getFeeRate()

  def start(): Future[WalletApi]

  def stop(): Future[WalletApi]

  /** Gets the sum of all UTXOs in this wallet */
  def getBalance()(implicit ec: ExecutionContext): Future[CurrencyUnit] = {
    val confirmedF = getConfirmedBalance()
    val unconfirmedF = getUnconfirmedBalance()

    for {
      confirmed <- confirmedF
      unconfirmed <- unconfirmedF
    } yield confirmed + unconfirmed
  }

  /** Gets the sum of all UTXOs in this wallet with the address tag */
  def getBalance(tag: AddressTag)(implicit
      ec: ExecutionContext): Future[CurrencyUnit] = {
    val confirmedF = getConfirmedBalance(tag)
    val unconfirmedF = getUnconfirmedBalance(tag)

    for {
      confirmed <- confirmedF
      unconfirmed <- unconfirmedF
    } yield confirmed + unconfirmed
  }

  /** Gets the sum of all confirmed UTXOs in this wallet */
  def getConfirmedBalance(): Future[CurrencyUnit]

  def getConfirmedBalance(tag: AddressTag): Future[CurrencyUnit]

  def getNewAddress(): Future[BitcoinAddress]

  def getNewChangeAddress(): Future[BitcoinAddress]

  /** Gets the sum of all unconfirmed UTXOs in this wallet */
  def getUnconfirmedBalance(): Future[CurrencyUnit]

  def getUnconfirmedBalance(tag: AddressTag): Future[CurrencyUnit]

  def listTransactions(): Future[Vector[TransactionDb]]

  def listUtxos(): Future[Vector[SpendingInfoDb]]

  def listUtxos(state: TxoState): Future[Vector[SpendingInfoDb]]

  def listUtxos(tag: AddressTag): Future[Vector[SpendingInfoDb]]

  /** Checks if the wallet contains any data */
  def isEmpty(): Future[Boolean]

  protected def determineFeeRate(feeRateOpt: Option[FeeUnit]): Future[FeeUnit] =
    feeRateOpt match {
      case None =>
        feeRateApi.getFeeRate()
      case Some(feeRate) =>
        Future.successful(feeRate)
    }

  /** Determines if the given output is from this wallet and is a change output
    * from this wallet
    */
  def isChange(output: TransactionOutput): Future[Boolean]

  def getSyncState(): Future[BlockSyncState]

  def isRescanning(): Future[Boolean]

  def getSyncDescriptorOpt(): Future[Option[SyncHeightDescriptor]]

  def getWalletName(): Future[String]

  def getInfo(): Future[WalletInfo]

  def findByOutPoints(
      outPoints: Vector[TransactionOutPoint]): Future[Vector[SpendingInfoDb]]

  def findByOutPoint(outPoint: TransactionOutPoint)(implicit
      ec: ExecutionContext): Future[Option[SpendingInfoDb]] = {
    findByOutPoints(Vector(outPoint)).map(_.headOption)
  }

  def findByTxIds(
      txIds: Vector[DoubleSha256DigestBE]): Future[Vector[TransactionDb]]

  def findByTxId(txId: DoubleSha256DigestBE)(implicit
      ec: ExecutionContext): Future[Option[TransactionDb]] = {
    findByTxIds(Vector(txId)).map(_.headOption)
  }

  def findByTxId(txId: DoubleSha256Digest)(implicit
      ec: ExecutionContext): Future[Option[TransactionDb]] = {
    findByTxId(txId.flip)
  }

  /** Finds all the outputs in our wallet being spent in the given transaction
    */
  def findOutputsBeingSpent(tx: Transaction): Future[Vector[SpendingInfoDb]]

  def findByScriptPubKey(
      scriptPubKey: ScriptPubKey): Future[Vector[SpendingInfoDb]]

  /** Takes in a block header and updates our TxoStates to the new chain tip
    * @param blockHeader
    *   Block header we are processing
    */
  def updateUtxoPendingStates(): Future[Vector[SpendingInfoDb]]

  def markUTXOsAsReserved(
      utxos: Vector[SpendingInfoDb]): Future[Vector[SpendingInfoDb]]

  /** Marks all utxos that are ours in this transactions as reserved */
  def markUTXOsAsReserved(tx: Transaction): Future[Vector[SpendingInfoDb]]

  def unmarkUTXOsAsReserved(
      utxos: Vector[SpendingInfoDb]): Future[Vector[SpendingInfoDb]]

  /** Unmarks all utxos that are ours in this transactions indicating they are
    * no longer reserved
    */
  def unmarkUTXOsAsReserved(tx: Transaction): Future[Vector[SpendingInfoDb]]
}

case class WalletInfo(
    walletName: String,
    rootXpub: ExtPublicKey,
    xpub: ExtPublicKey,
    hdAccount: HDAccount,
    height: Int,
    blockHash: DoubleSha256DigestBE,
    rescan: Boolean,
    imported: Boolean)

/** An HDWallet that uses Neutrino to sync */
trait NeutrinoHDWalletApi extends WalletApi with NeutrinoWalletApi
