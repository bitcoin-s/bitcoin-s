package org.bitcoins.core.api.wallet

import org.bitcoins.core.api.chain.ChainQueryApi
import org.bitcoins.core.api.feeprovider.FeeRateApi
import org.bitcoins.core.api.keymanager.KeyManagerApi
import org.bitcoins.core.api.node.NodeApi
import org.bitcoins.core.api.wallet.db._
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.hd.AddressType
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.{
  Transaction,
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.util.{FutureUtil, StartStopAsync}
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.utxo.{AddressTag, AddressTagType, TxoState}
import org.bitcoins.crypto.DoubleSha256DigestBE

import java.time.Instant
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/**
  * API for the wallet project.
  *
  * This wallet API is BIP44 compliant.
  *
  * @see [[https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki BIP44]]
  */
trait WalletApi extends StartStopAsync[WalletApi] {

  val nodeApi: NodeApi
  val chainQueryApi: ChainQueryApi
  val feeRateApi: FeeRateApi
  val creationTime: Instant

  def broadcastTransaction(transaction: Transaction): Future[Unit] =
    nodeApi.broadcastTransaction(transaction)

  def getFeeRate: Future[FeeUnit] = feeRateApi.getFeeRate

  def start(): Future[WalletApi]

  def stop(): Future[WalletApi]

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

  def findTransaction(txId: DoubleSha256DigestBE): Future[Option[TransactionDb]]

  def listTransactions(): Future[Vector[TransactionDb]]

  /**
    * Takes in a block header and updates our TxoStates to the new chain tip
    * @param blockHeader Block header we are processing
    */
  def updateUtxoPendingStates(): Future[Vector[SpendingInfoDb]]

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

  def listScriptPubKeys(): Future[Vector[ScriptPubKeyDb]]

  def watchScriptPubKey(scriptPubKey: ScriptPubKey): Future[ScriptPubKeyDb]

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
  def getNewAddress(): Future[BitcoinAddress]

  def getNewAddress(
      addressType: AddressType,
      tags: Vector[AddressTag]): Future[BitcoinAddress]

  def getNewAddress(tags: Vector[AddressTag]): Future[BitcoinAddress]

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
      spendingInfoDb: SpendingInfoDb,
      networkParameters: NetworkParameters): Future[Option[AddressInfo]] = {
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

  /** Tags the address with the address tag, updates the tag if one of tag's TagType already exists */
  def tagAddress(address: BitcoinAddress, tag: AddressTag): Future[AddressTagDb]

  def getAddressTags(address: BitcoinAddress): Future[Vector[AddressTagDb]]

  def getAddressTags(
      address: BitcoinAddress,
      tagType: AddressTagType): Future[Vector[AddressTagDb]]

  def getAddressTags: Future[Vector[AddressTagDb]]

  def getAddressTags(tagType: AddressTagType): Future[Vector[AddressTagDb]]

  def dropAddressTag(addressTagDb: AddressTagDb): Future[Int]

  def dropAddressTagType(addressTagType: AddressTagType): Future[Int]

  def dropAddressTagType(
      address: BitcoinAddress,
      addressTagType: AddressTagType): Future[Int]

  /** Generates a new change address */
  protected[wallet] def getNewChangeAddress()(implicit
      ec: ExecutionContext): Future[BitcoinAddress]

  def keyManager: KeyManagerApi

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

  def sendFromOutPoints(
      outPoints: Vector[TransactionOutPoint],
      address: BitcoinAddress,
      feeRate: FeeUnit)(implicit ec: ExecutionContext): Future[Transaction]

  def sendFromOutPoints(
      outPoints: Vector[TransactionOutPoint],
      address: BitcoinAddress,
      feeRateOpt: Option[FeeUnit]
  )(implicit ec: ExecutionContext): Future[Transaction] = {
    for {
      feeRate <- determineFeeRate(feeRateOpt)
      tx <- sendFromOutPoints(outPoints, address, feeRate)
    } yield tx
  }

  def emptyWallet(address: BitcoinAddress, feeRateOpt: Option[FeeUnit])(implicit
      ec: ExecutionContext): Future[Transaction] = {
    for {
      feeRate <- determineFeeRate(feeRateOpt)
      utxos <- listUtxos()
      outPoints = utxos.map(_.outPoint)
      tx <- sendFromOutPoints(outPoints, address, feeRate)
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

  def bumpFeeRBF(
      txId: DoubleSha256DigestBE,
      newFeeRate: FeeUnit): Future[Transaction]

  /** Bumps the fee of the parent transaction with a new
    * child transaction with the given fee rate
    */
  def bumpFeeCPFP(
      txId: DoubleSha256DigestBE,
      feeRate: FeeUnit): Future[Transaction]

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

  /** Determines if the given output is from this wallet and
    * is a change output from this wallet
    */
  def isChange(output: TransactionOutput): Future[Boolean]

  def getSyncState(): Future[BlockSyncState]
}

/** An HDWallet that uses Neutrino to sync */
trait NeutrinoHDWalletApi extends HDWalletApi with NeutrinoWalletApi

/** An HDWallet that uses SPV to sync */
trait SpvHDWalletApi extends HDWalletApi with SpvWalletApi

/** An HDWallet that supports both Neutrino and SPV methods of syncing */
trait AnyHDWalletApi
    extends HDWalletApi
    with NeutrinoWalletApi
    with SpvWalletApi
