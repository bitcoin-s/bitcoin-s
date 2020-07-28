package org.bitcoins.wallet.api

import java.time.Instant

import org.bitcoins.commons.jsonmodels.wallet.CoinSelectionAlgo
import org.bitcoins.core.api.{ChainQueryApi, FeeRateApi, NodeApi}
import org.bitcoins.core.bloom.BloomFilter
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.gcs.GolombFilter
import org.bitcoins.core.hd.AddressType
import org.bitcoins.core.protocol.blockchain.{Block, BlockHeader, ChainParams}
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.{
  Transaction,
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.protocol.{BitcoinAddress, BlockStamp}
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.utxo.{AddressTag, TxoState}
import org.bitcoins.crypto.{
  AesPassword,
  DoubleSha256Digest,
  DoubleSha256DigestBE
}
import org.bitcoins.keymanager._
import org.bitcoins.keymanager.bip39.BIP39KeyManager
import org.bitcoins.wallet.WalletLogger
import org.bitcoins.wallet.api.WalletApi.BlockMatchingResponse
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.models.{AddressDb, SpendingInfoDb}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/**
  * API for the wallet project.
  *
  * This wallet API is BIP44 compliant.
  *
  * @see [[https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki BIP44]]
  */
trait WalletApi extends WalletLogger {

  implicit val walletConfig: WalletAppConfig

  val nodeApi: NodeApi
  val chainQueryApi: ChainQueryApi
  val feeRateApi: FeeRateApi
  val creationTime: Instant

  def chainParams: ChainParams = walletConfig.chain

  def networkParameters: NetworkParameters = walletConfig.network

  def broadcastTransaction(transaction: Transaction): Future[Unit] =
    nodeApi.broadcastTransaction(transaction)

  def start(): Future[Unit]

  def stop(): Unit

  /**
    * Retrieves a bloom filter that that can be sent to a P2P network node
    * to get information about our transactions, pubkeys and scripts.
    */
  def getBloomFilter(): Future[BloomFilter]

  /**
    * Processes the given transaction, updating our DB state if it's relevant to us.
    * @param transaction The transaction we're processing
    * @param blockHash Containing block hash
    */
  def processTransaction(
      transaction: Transaction,
      blockHash: Option[DoubleSha256DigestBE]): Future[WalletApi]

  def processTransactions(
      transactions: Vector[Transaction],
      blockHash: Option[DoubleSha256DigestBE])(implicit
      ec: ExecutionContext): Future[WalletApi] = {
    transactions.foldLeft(Future.successful(this)) {
      case (wallet, tx) =>
        wallet.flatMap(_.processTransaction(tx, blockHash))
    }
  }

  /**
    * Takes in a block header and updates our TxoStates to the new chain tip
    * @param blockHeader Block header we are processing
    */
  def updateUtxoPendingStates(
      blockHeader: BlockHeader): Future[Vector[SpendingInfoDb]]

  /**
    * Processes the give block, updating our DB state if it's relevant to us.
    * @param block The block we're processing
    */
  def processBlock(block: Block): Future[WalletApi]

  def processCompactFilter(
      blockHash: DoubleSha256Digest,
      blockFilter: GolombFilter): Future[WalletApi] =
    processCompactFilters(Vector((blockHash, blockFilter)))

  def processCompactFilters(
      blockFilters: Vector[(DoubleSha256Digest, GolombFilter)]): Future[
    WalletApi]

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

  /** Gets the sum of all unconfirmed UTXOs in this wallet */
  def getUnconfirmedBalance(): Future[CurrencyUnit]

  def getUnconfirmedBalance(tag: AddressTag): Future[CurrencyUnit]

  /**
    * If a UTXO is spent outside of the wallet, we
    * need to remove it from the database so it won't be
    * attempted spent again by us.
    */
  // def updateUtxo: Future[WalletApi]

  /** Lists unspent transaction outputs in the wallet
    * @return Vector[SpendingInfoDb]
    */
  def listUtxos(): Future[Vector[SpendingInfoDb]]

  def listUtxos(tag: AddressTag): Future[Vector[SpendingInfoDb]]

  def listUtxos(state: TxoState): Future[Vector[SpendingInfoDb]]

  def listAddresses(): Future[Vector[AddressDb]]

  def listSpentAddresses(): Future[Vector[AddressDb]]

  def listFundedAddresses(): Future[Vector[(AddressDb, CurrencyUnit)]]

  def listUnusedAddresses(): Future[Vector[AddressDb]]

  def markUTXOsAsReserved(
      utxos: Vector[SpendingInfoDb]): Future[Vector[SpendingInfoDb]]

  /** Marks all utxos that are ours in this transactions as reserved */
  def markUTXOsAsReserved(tx: Transaction): Future[Vector[SpendingInfoDb]]

  def unmarkUTXOsAsReserved(
      utxos: Vector[SpendingInfoDb]): Future[Vector[SpendingInfoDb]]

  /** Unmarks all utxos that are ours in this transactions indicating they are no longer reserved */
  def unmarkUTXOsAsReserved(tx: Transaction): Future[Vector[SpendingInfoDb]]

  /** Checks if the wallet contains any data */
  def isEmpty(): Future[Boolean]

  /** Removes all utxos and addresses from the wallet.
    * Don't call this unless you are sure you can recover
    * your wallet
    */
  def clearAllUtxosAndAddresses(): Future[WalletApi]

  /**
    * Gets a new external address with the specified
    * type.
    *  @param addressType
    */
  def getNewAddress(addressType: AddressType): Future[BitcoinAddress]

  /**
    * Gets a new external address
    * Calling this method multiple
    * times will return the same address, until it has
    * received funds.
    */
  def getNewAddress()(implicit ec: ExecutionContext): Future[BitcoinAddress] = {
    for {
      address <- getNewAddress(walletConfig.defaultAddressType)
    } yield address
  }

  def getNewAddress(
      addressType: AddressType,
      tags: Vector[AddressTag]): Future[BitcoinAddress]

  def getNewAddress(tags: Vector[AddressTag])(implicit
      ec: ExecutionContext): Future[BitcoinAddress] = {
    for {
      address <- getNewAddress(walletConfig.defaultAddressType, tags)
    } yield address
  }

  /**
    * Gets a external address the given AddressType. Calling this
    * method multiple times will return the same address, until it has
    * received funds.
    */
  def getUnusedAddress(addressType: AddressType): Future[BitcoinAddress]

  /**
    * Gets a external address. Calling this method multiple
    * times will return the same address, until it has
    * received funds.
    */
  def getUnusedAddress: Future[BitcoinAddress]

  /**
    * Mimics the `getaddressinfo` RPC call in Bitcoin Core
    *
    * @param address
    * @return If the address is found in our database `Some(address)`
    *         is returned, otherwise `None`
    */
  def getAddressInfo(address: BitcoinAddress): Future[Option[AddressInfo]]

  def getAddressInfo(
      spendingInfoDb: SpendingInfoDb): Future[Option[AddressInfo]] = {
    val addressT = BitcoinAddress.fromScriptPubKeyT(
      spk = spendingInfoDb.output.scriptPubKey,
      np = networkParameters)
    addressT match {
      case Success(addr) =>
        getAddressInfo(addr)
      case Failure(_) =>
        FutureUtil.none
    }
  }

  /** Generates a new change address */
  protected[wallet] def getNewChangeAddress()(implicit
      ec: ExecutionContext): Future[BitcoinAddress]

  /**
    * Unlocks the wallet with the provided passphrase,
    * making it possible to send transactions.
    */
  def unlock(passphrase: AesPassword, bip39PasswordOpt: Option[String]): Either[
    KeyManagerUnlockError,
    WalletApi]

  /**
    * Iterates over the block filters in order to find filters that match to the given addresses
    *
    * I queries the filter database for [[batchSize]] filters a time
    * and tries to run [[GolombFilter.matchesAny]] for each filter.
    *
    * It tries to match the filters in parallel using [[parallelismLevel]] threads.
    * For best results use it with a separate execution context.
    *
    * @param scripts list of [[ScriptPubKey]]'s to watch
    * @param startOpt start point (if empty it starts with the genesis block)
    * @param endOpt end point (if empty it ends with the best tip)
    * @param batchSize number of filters that can be matched in one batch
    * @param parallelismLevel max number of threads required to perform matching
    *                         (default [[Runtime.getRuntime.availableProcessors()]])
    * @return a list of matching block hashes
    */
  def getMatchingBlocks(
      scripts: Vector[ScriptPubKey],
      startOpt: Option[BlockStamp] = None,
      endOpt: Option[BlockStamp] = None,
      batchSize: Int = 100,
      parallelismLevel: Int = Runtime.getRuntime.availableProcessors())(implicit
      ec: ExecutionContext): Future[Vector[BlockMatchingResponse]]

  /**
    * Recreates the account using BIP-157 approach
    *
    * DANGER! This method removes all records from the wallet database
    * and creates new ones while the account discovery process.
    *
    * The Wallet UI should check if the database is empty before calling
    * this method and let the end users to decide whether they want to proceed or not.
    *
    * This method generates [[addressBatchSize]] of addresses, then matches them against the BIP-158 compact filters,
    * and downloads and processes the matched blocks. This method keeps doing the steps until there are [[WalletConfig.addressGapLimit]]
    * or more unused addresses in a row. In this case it considers the discovery process completed.
    *
    * [[addressBatchSize]] - the number of addresses we should generate from a keychain to attempt to match in in a rescan
    * [[WalletConfig.addressGapLimit]] - the number of addresses required to go without a match before we determine that our wallet is "discovered".
    * For instance, if addressBatchSize=100, and AddressGapLimit=20 we do a rescan and the last address we find containing
    * funds is at index 75, we would not generate more addresses to try and rescan. However if the last index containing
    * funds was 81, we would generate another 100 addresses from the keychain and attempt to rescan those.
    *
    * @param startOpt start block (if None it starts from the genesis block)
    * @param endOpt end block (if None it ends at the current tip)
    * @param addressBatchSize how many addresses to match in a single pass
    */

  def rescanNeutrinoWallet(
      startOpt: Option[BlockStamp],
      endOpt: Option[BlockStamp],
      addressBatchSize: Int,
      useCreationTime: Boolean)(implicit ec: ExecutionContext): Future[Unit]

  /** Helper method to rescan the ENTIRE blockchain. */
  def fullRescanNeutrinoWallet(addressBatchSize: Int)(implicit
      ec: ExecutionContext): Future[Unit]

  /**
    * Recreates the account using BIP-44 approach
    */
  def rescanSPVWallet(): Future[Unit]

  def discoveryBatchSize(): Int = walletConfig.discoveryBatchSize

  def keyManager: BIP39KeyManager

  protected def determineFeeRate(feeRateOpt: Option[FeeUnit]): Future[FeeUnit] =
    feeRateOpt match {
      case None =>
        feeRateApi.getFeeRate
      case Some(feeRate) =>
        Future.successful(feeRate)
    }

  def sendFromOutPoints(
      outPoints: Vector[TransactionOutPoint],
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRate: FeeUnit)(implicit ec: ExecutionContext): Future[Transaction]

  def sendFromOutPoints(
      outPoints: Vector[TransactionOutPoint],
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRateOpt: Option[FeeUnit]
  )(implicit ec: ExecutionContext): Future[Transaction] = {
    for {
      feeRate <- determineFeeRate(feeRateOpt)
      tx <- sendFromOutPoints(outPoints, address, amount, feeRate)
    } yield tx
  }

  def sendWithAlgo(
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRate: FeeUnit,
      algo: CoinSelectionAlgo)(implicit
      ec: ExecutionContext): Future[Transaction]

  def sendWithAlgo(
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRateOpt: Option[FeeUnit],
      algo: CoinSelectionAlgo
  )(implicit ec: ExecutionContext): Future[Transaction] = {
    for {
      feeRate <- determineFeeRate(feeRateOpt)
      tx <- sendWithAlgo(address, amount, feeRate, algo)
    } yield tx
  }

  /**
    * Sends money to the address
    *
    * todo: add error handling to signature
    */
  def sendToAddress(
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRate: FeeUnit)(implicit ec: ExecutionContext): Future[Transaction]

  def sendToAddress(
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRateOpt: Option[FeeUnit]
  )(implicit ec: ExecutionContext): Future[Transaction] = {
    for {
      feeRate <- determineFeeRate(feeRateOpt)
      tx <- sendToAddress(address, amount, feeRate)
    } yield tx
  }

  /**
    * Sends funds using the specified outputs
    *
    * todo: add error handling to signature
    */
  def sendToOutputs(
      outputs: Vector[TransactionOutput],
      feeRateOpt: Option[FeeUnit])(implicit
      ec: ExecutionContext): Future[Transaction] = {
    for {
      feeRate <- determineFeeRate(feeRateOpt)
      tx <- sendToOutputs(outputs, feeRate)
    } yield tx
  }

  def sendToOutputs(outputs: Vector[TransactionOutput], feeRate: FeeUnit)(
      implicit ec: ExecutionContext): Future[Transaction]

  /**
    * Sends funds to each address
    */
  def sendToAddresses(
      addresses: Vector[BitcoinAddress],
      amounts: Vector[CurrencyUnit],
      feeRateOpt: Option[FeeUnit])(implicit
      ec: ExecutionContext): Future[Transaction] = {
    for {
      feeRate <- determineFeeRate(feeRateOpt)
      tx <- sendToAddresses(addresses, amounts, feeRate)
    } yield tx
  }

  def sendToAddresses(
      addresses: Vector[BitcoinAddress],
      amounts: Vector[CurrencyUnit],
      feeRate: FeeUnit)(implicit ec: ExecutionContext): Future[Transaction]

  def makeOpReturnCommitment(
      message: String,
      hashMessage: Boolean,
      feeRate: FeeUnit)(implicit ec: ExecutionContext): Future[Transaction]

  def makeOpReturnCommitment(
      message: String,
      hashMessage: Boolean,
      feeRateOpt: Option[FeeUnit])(implicit
      ec: ExecutionContext): Future[Transaction] = {
    for {
      feeRate <- determineFeeRate(feeRateOpt)
      tx <- makeOpReturnCommitment(message, hashMessage, feeRate)
    } yield tx
  }

}

object WalletApi {

  case class BlockMatchingResponse(
      blockHash: DoubleSha256DigestBE,
      blockHeight: Int)

}
