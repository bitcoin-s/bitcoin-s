package org.bitcoins.wallet.api

import java.time.Instant

import org.bitcoins.commons.jsonmodels.wallet.CoinSelectionAlgo
import org.bitcoins.commons.jsonmodels.dlc.DLCMessage._
import org.bitcoins.core.api.{ChainQueryApi, FeeRateApi, NodeApi}
import org.bitcoins.core.bloom.BloomFilter
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.gcs.{GolombFilter, SimpleFilterMatcher}
import org.bitcoins.core.hd.{AddressType, HDAccount, HDChainType, HDPurpose}
import org.bitcoins.core.number.UInt32
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
import org.bitcoins.crypto._
import org.bitcoins.keymanager._
import org.bitcoins.keymanager.bip39.{BIP39KeyManager, BIP39LockedKeyManager}
import org.bitcoins.wallet.api.WalletApi.BlockMatchingResponse
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.models.{AccountDb, AddressDb, DLCDb, SpendingInfoDb}
import org.bitcoins.wallet.{Wallet, WalletLogger}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/**
  * API for the wallet project.
  *
  * This wallet API is BIP344 compliant.
  *
  * @see [[https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki BIP44]]
  */
trait WalletApi extends WalletLogger {

  implicit val walletConfig: WalletAppConfig
  implicit val ec: ExecutionContext

  val nodeApi: NodeApi
  val chainQueryApi: ChainQueryApi
  val feeRateApi: FeeRateApi
  val creationTime: Instant

  def chainParams: ChainParams = walletConfig.chain

  def networkParameters: NetworkParameters = walletConfig.network

  def decodeRawTransaction(tx: Transaction): String =
    SerializedTransaction.decodeRawTransaction(tx)

  def broadcastTransaction(transaction: Transaction): Future[Unit] =
    nodeApi.broadcastTransaction(transaction)

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
      blockHash: Option[DoubleSha256DigestBE]): Future[WalletApi] = {
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
    WalletApi] = {
    val utxosF = listUtxos()
    val addressesF = listAddresses()
    for {
      utxos <- utxosF
      addresses <- addressesF
      scriptPubKeys =
        utxos.flatMap(_.redeemScriptOpt).toSet ++ addresses
          .map(_.scriptPubKey)
          .toSet
      _ <- FutureUtil.sequentially(blockFilters) {
        case (blockHash, blockFilter) =>
          val matcher = SimpleFilterMatcher(blockFilter)
          if (matcher.matchesAny(scriptPubKeys.toVector.map(_.asmBytes))) {
            nodeApi.downloadBlocks(Vector(blockHash))
          } else FutureUtil.unit
      }
    } yield {
      this
    }
  }

  /** Gets the sum of all UTXOs in this wallet */
  def getBalance(): Future[CurrencyUnit] = {
    val confirmedF = getConfirmedBalance()
    val unconfirmedF = getUnconfirmedBalance()

    for {
      confirmed <- confirmedF
      unconfirmed <- unconfirmedF
    } yield confirmed + unconfirmed
  }

  /** Gets the balance of the given account */
  def getBalance(account: HDAccount): Future[CurrencyUnit] = {
    val confirmedF = getConfirmedBalance(account)
    val unconfirmedF = getUnconfirmedBalance(account)
    for {
      confirmed <- confirmedF
      unconfirmed <- unconfirmedF
    } yield {
      confirmed + unconfirmed
    }
  }

  /** Gets the sum of all confirmed UTXOs in this wallet */
  def getConfirmedBalance(): Future[CurrencyUnit]

  def getConfirmedBalance(account: HDAccount): Future[CurrencyUnit]

  /** Gets the sum of all unconfirmed UTXOs in this wallet */
  def getUnconfirmedBalance(): Future[CurrencyUnit]

  def getUnconfirmedBalance(account: HDAccount): Future[CurrencyUnit]

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

  def listUtxos(account: HDAccount): Future[Vector[SpendingInfoDb]]

  def listAddresses(): Future[Vector[AddressDb]]

  def listAddresses(account: HDAccount): Future[Vector[AddressDb]]

  def listSpentAddresses(): Future[Vector[AddressDb]]

  def listSpentAddresses(account: HDAccount): Future[Vector[AddressDb]]

  def listFundedAddresses(): Future[Vector[(AddressDb, CurrencyUnit)]]

  def listFundedAddresses(
      account: HDAccount): Future[Vector[(AddressDb, CurrencyUnit)]]

  def listUnusedAddresses(): Future[Vector[AddressDb]]

  def listUnusedAddresses(account: HDAccount): Future[Vector[AddressDb]]

  def markUTXOsAsReserved(
      utxos: Vector[SpendingInfoDb]): Future[Vector[SpendingInfoDb]]

  def unmarkUTXOsAsReserved(
      utxos: Vector[SpendingInfoDb]): Future[Vector[SpendingInfoDb]]

  /** Unmarks all utxos that are ours in this transactions indicating they are no longer reserved */
  def unmarkUTXOsAsReserved(tx: Transaction): Future[Vector[SpendingInfoDb]]

  /** Checks if the wallet contains any data */
  def isEmpty(): Future[Boolean]

  /** Removes all utxos and addresses from the wallet account.
    * Don't call this unless you are sure you can recover
    * your wallet
    */
  def clearUtxosAndAddresses(account: HDAccount): Future[WalletApi]

  def clearUtxosAndAddresses(): Future[WalletApi] =
    clearUtxosAndAddresses(walletConfig.defaultAccount)

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
    * Gets a new external address from the default account.
    * Calling this method multiple
    * times will return the same address, until it has
    * received funds.
    */
  def getNewAddress(): Future[BitcoinAddress] = {
    for {
      address <- getNewAddress(walletConfig.defaultAddressType)
    } yield address
  }

  /**
    * Gets a external address from the account associated with
    * the given AddressType. Calling this method multiple
    * times will return the same address, until it has
    * received funds.
    */
  def getUnusedAddress(addressType: AddressType): Future[BitcoinAddress]

  /**
    * Gets a external address from the default account.
    * Calling this method multiple
    * times will return the same address, until it has
    * received funds.
    */
  def getUnusedAddress: Future[BitcoinAddress]

  /** Gets the address associated with the pubkey at
    * the resulting `BIP32Path` determined by the
    * default account and the given chainType and addressIndex
    */
  def getAddress(
      chainType: HDChainType,
      addressIndex: Int): Future[AddressDb] = {
    for {
      account <- getDefaultAccount()
      address <- getAddress(account, chainType, addressIndex)
    } yield address
  }

  /** Gets the address associated with the pubkey at
    * the resulting `BIP32Path` determined the given
    * account, chainType, and addressIndex
    */
  def getAddress(
      account: AccountDb,
      chainType: HDChainType,
      addressIndex: Int): Future[AddressDb]

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
  protected[wallet] def getNewChangeAddress(
      account: AccountDb): Future[BitcoinAddress]

  /** Generates a new change address for the default account */
  final protected[wallet] def getNewChangeAddress(): Future[BitcoinAddress] = {
    for {
      account <- getDefaultAccount
      address <- getNewChangeAddress(account)

    } yield address
  }

  /**
    * Fetches the default account from the DB
    * @return Future[AccountDb]
    */
  protected[wallet] def getDefaultAccount(): Future[AccountDb]

  /** Fetches the default account for the given address/account kind
    * @param addressType
    */
  protected[wallet] def getDefaultAccountForType(
      addressType: AddressType): Future[AccountDb]

  /**
    * Unlocks the wallet with the provided passphrase,
    * making it possible to send transactions.
    */
  def unlock(passphrase: AesPassword, bip39PasswordOpt: Option[String]): Either[
    KeyManagerUnlockError,
    WalletApi] = {
    val kmParams = walletConfig.kmParams

    val unlockedKeyManagerE =
      BIP39LockedKeyManager.unlock(passphrase = passphrase,
                                   bip39PasswordOpt = bip39PasswordOpt,
                                   kmParams = kmParams)
    unlockedKeyManagerE match {
      case Right(km) =>
        val w = Wallet(keyManager = km,
                       nodeApi = nodeApi,
                       chainQueryApi = chainQueryApi,
                       feeRateApi = feeRateApi,
                       creationTime = km.creationTime)
        Right(w)
      case Left(err) => Left(err)
    }
  }

  def listAccounts(): Future[Vector[AccountDb]]

  /** Lists all wallet accounts with the given type
    * @param purpose
    * @return [[Future[Vector[AccountDb]]
    */
  def listAccounts(purpose: HDPurpose): Future[Vector[AccountDb]] =
    listAccounts().map(_.filter(_.hdAccount.purpose == purpose))

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
      account: HDAccount,
      startOpt: Option[BlockStamp],
      endOpt: Option[BlockStamp],
      addressBatchSize: Int,
      useCreationTime: Boolean): Future[Unit]

  def rescanNeutrinoWallet(
      startOpt: Option[BlockStamp],
      endOpt: Option[BlockStamp],
      addressBatchSize: Int,
      useCreationTime: Boolean): Future[Unit] =
    rescanNeutrinoWallet(account = walletConfig.defaultAccount,
                         startOpt = startOpt,
                         endOpt = endOpt,
                         addressBatchSize = addressBatchSize,
                         useCreationTime = useCreationTime)

  /** Helper method to rescan the ENTIRE blockchain. */
  def fullRescanNeutrinoWallet(addressBatchSize: Int): Future[Unit] =
    fullRescanNeutrinoWallet(account = walletConfig.defaultAccount,
                             addressBatchSize = addressBatchSize)

  def fullRescanNeutrinoWallet(
      account: HDAccount,
      addressBatchSize: Int): Future[Unit] =
    rescanNeutrinoWallet(account = account,
                         startOpt = None,
                         endOpt = None,
                         addressBatchSize = addressBatchSize,
                         useCreationTime = false)

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
      feeRate: FeeUnit,
      fromAccount: AccountDb): Future[Transaction]

  def sendFromOutPoints(
      outPoints: Vector[TransactionOutPoint],
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRateOpt: Option[FeeUnit],
      fromAccount: AccountDb): Future[Transaction] = {
    for {
      feeRate <- determineFeeRate(feeRateOpt)
      tx <- sendFromOutPoints(outPoints, address, amount, feeRate, fromAccount)
    } yield tx
  }

  def sendFromOutPoints(
      outPoints: Vector[TransactionOutPoint],
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRateOpt: Option[FeeUnit]): Future[Transaction] = {
    for {
      account <- getDefaultAccount()
      tx <- sendFromOutPoints(outPoints, address, amount, feeRateOpt, account)
    } yield tx
  }

  def sendWithAlgo(
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRate: FeeUnit,
      algo: CoinSelectionAlgo,
      fromAccount: AccountDb): Future[Transaction]

  def sendWithAlgo(
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRateOpt: Option[FeeUnit],
      algo: CoinSelectionAlgo,
      fromAccount: AccountDb): Future[Transaction] = {
    for {
      feeRate <- determineFeeRate(feeRateOpt)
      tx <- sendWithAlgo(address, amount, feeRate, algo, fromAccount)
    } yield tx
  }

  def sendWithAlgo(
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRateOpt: Option[FeeUnit],
      algo: CoinSelectionAlgo): Future[Transaction] = {
    for {
      account <- getDefaultAccount()
      tx <- sendWithAlgo(address, amount, feeRateOpt, algo, account)
    } yield tx
  }

  def createDLCOffer(
      oracleInfo: OracleInfo,
      contractInfo: ContractInfo,
      collateral: Satoshis,
      feeRateOpt: Option[FeeUnit],
      locktime: UInt32,
      refundLT: UInt32): Future[DLCOffer]

  def registerDLCOffer(dlcOffer: DLCOffer): Future[DLCOffer] = {
    createDLCOffer(
      dlcOffer.oracleInfo,
      dlcOffer.contractInfo,
      dlcOffer.totalCollateral,
      Some(dlcOffer.feeRate),
      dlcOffer.timeouts.contractMaturity.toUInt32,
      dlcOffer.timeouts.contractTimeout.toUInt32
    )
  }

  def acceptDLCOffer(dlcOffer: DLCOffer): Future[DLCAccept]

  def signDLC(accept: DLCAccept): Future[DLCSign]

  def addDLCSigs(sigs: DLCSign): Future[DLCDb]

  def initDLCMutualClose(
      eventId: Sha256DigestBE,
      oracleSig: SchnorrDigitalSignature): Future[DLCMutualCloseSig]

  def acceptDLCMutualClose(
      mutualCloseSig: DLCMutualCloseSig): Future[Transaction]

  def getDLCFundingTx(eventId: Sha256DigestBE): Future[Transaction]

  def executeDLCUnilateralClose(
      eventId: Sha256DigestBE,
      oracleSig: SchnorrDigitalSignature): Future[
    (Transaction, Option[Transaction])]

  def executeRemoteUnilateralDLC(
      eventId: Sha256DigestBE,
      cet: Transaction): Future[Option[Transaction]]

  def executeDLCForceClose(
      eventId: Sha256DigestBE,
      oracleSig: SchnorrDigitalSignature): Future[
    (Transaction, Option[Transaction])]

  def claimDLCRemoteFunds(
      eventId: Sha256DigestBE,
      forceCloseTx: Transaction): Future[Option[Transaction]]

  def executeDLCRefund(
      eventId: Sha256DigestBE): Future[(Transaction, Option[Transaction])]

  def claimDLCPenaltyFunds(
      eventId: Sha256DigestBE,
      forceCloseTx: Transaction): Future[Option[Transaction]]

  /**
    * Sends money from the specified account
    *
    * todo: add error handling to signature
    */
  def sendToAddress(
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRate: FeeUnit,
      fromAccount: AccountDb): Future[Transaction]

  /**
    * Sends money from the specified account
    *
    * todo: add error handling to signature
    */
  def sendToAddress(
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRateOpt: Option[FeeUnit],
      fromAccount: AccountDb): Future[Transaction] = {
    for {
      feeRate <- determineFeeRate(feeRateOpt)
      tx <- sendToAddress(address, amount, feeRate, fromAccount)
    } yield tx
  }

  /**
    * Sends money from the default account
    *
    * todo: add error handling to signature
    */
  def sendToAddress(
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRateOpt: Option[FeeUnit]
  ): Future[Transaction] = {
    for {
      account <- getDefaultAccount()
      tx <- sendToAddress(address, amount, feeRateOpt, account)
    } yield tx
  }

  /**
    * Sends money from the specified account
    *
    * todo: add error handling to signature
    */
  def sendToOutputs(
      outputs: Vector[TransactionOutput],
      feeRate: FeeUnit,
      fromAccount: AccountDb,
      reserveUtxos: Boolean): Future[Transaction]

  def sendToOutputs(
      outputs: Vector[TransactionOutput],
      feeRateOpt: Option[FeeUnit],
      fromAccount: AccountDb,
      reserveUtxos: Boolean): Future[Transaction] = {
    for {
      feeRate <- determineFeeRate(feeRateOpt)
      tx <- sendToOutputs(outputs, feeRate, fromAccount, reserveUtxos)
    } yield tx
  }

  /**
    * Sends money from the default account
    *
    * todo: add error handling to signature
    */
  def sendToOutputs(
      outputs: Vector[TransactionOutput],
      feeRateOpt: Option[FeeUnit],
      reserveUtxos: Boolean): Future[Transaction] = {
    for {
      account <- getDefaultAccount()
      tx <- sendToOutputs(outputs, feeRateOpt, account, reserveUtxos)
    } yield tx
  }

  /**
    * Sends money from the specified account
    *
    * todo: add error handling to signature
    */
  def sendToAddresses(
      addresses: Vector[BitcoinAddress],
      amounts: Vector[CurrencyUnit],
      feeRate: FeeUnit,
      fromAccount: AccountDb,
      reserveUtxos: Boolean): Future[Transaction]

  /**
    * Sends money from the specified account
    *
    * todo: add error handling to signature
    */
  def sendToAddresses(
      addresses: Vector[BitcoinAddress],
      amounts: Vector[CurrencyUnit],
      feeRateOpt: Option[FeeUnit],
      fromAccount: AccountDb,
      reserveUtxos: Boolean): Future[Transaction] = {
    for {
      feeRate <- determineFeeRate(feeRateOpt)
      tx <-
        sendToAddresses(addresses, amounts, feeRate, fromAccount, reserveUtxos)
    } yield tx
  }

  /**
    * Sends money from the default account
    *
    * todo: add error handling to signature
    */
  def sendToAddresses(
      addresses: Vector[BitcoinAddress],
      amounts: Vector[CurrencyUnit],
      feeRateOpt: Option[FeeUnit],
      reserveUtxos: Boolean): Future[Transaction] = {
    for {
      account <- getDefaultAccount()
      tx <-
        sendToAddresses(addresses, amounts, feeRateOpt, account, reserveUtxos)
    } yield tx
  }

  def makeOpReturnCommitment(
      message: String,
      hashMessage: Boolean,
      feeRate: FeeUnit,
      fromAccount: AccountDb): Future[Transaction]

  def makeOpReturnCommitment(
      message: String,
      hashMessage: Boolean,
      feeRateOpt: Option[FeeUnit],
      fromAccount: AccountDb): Future[Transaction] = {
    for {
      feeRate <- determineFeeRate(feeRateOpt)
      tx <- makeOpReturnCommitment(message, hashMessage, feeRate, fromAccount)
    } yield tx
  }

  def makeOpReturnCommitment(
      message: String,
      hashMessage: Boolean,
      feeRateOpt: Option[FeeUnit]): Future[Transaction] = {
    for {
      account <- getDefaultAccount()
      tx <- makeOpReturnCommitment(message, hashMessage, feeRateOpt, account)
    } yield tx
  }

  def createNewAccount(keyManagerParams: KeyManagerParams): Future[Wallet]

  /**
    * Tries to create a new account in this wallet. Fails if the
    * most recent account has no transaction history, as per
    * BIP44
    *
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki#account BIP44 account section]]
    */
  def createNewAccount(
      hdAccount: HDAccount,
      keyManagerParams: KeyManagerParams): Future[Wallet]

}

object WalletApi {

  case class BlockMatchingResponse(
      blockHash: DoubleSha256DigestBE,
      blockHeight: Int)

}
