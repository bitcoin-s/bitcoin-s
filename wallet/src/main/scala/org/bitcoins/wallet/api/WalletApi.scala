package org.bitcoins.wallet.api

import java.util.concurrent.Executors

import org.bitcoins.core.api.ChainQueryApi.InvalidBlockRange
import org.bitcoins.core.api.{ChainQueryApi, NodeApi}
import org.bitcoins.core.bloom.BloomFilter
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.crypto._
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.gcs.{GolombFilter, SimpleFilterMatcher}
import org.bitcoins.core.hd.{AddressType, HDPurpose}
import org.bitcoins.core.protocol.blockchain.{Block, ChainParams}
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.protocol.{BitcoinAddress, BlockStamp}
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.keymanager._
import org.bitcoins.wallet.Wallet
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.models.{AccountDb, AddressDb, SpendingInfoDb}

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}

/**
  * API for the wallet project.
  *
  * This wallet API is BIP344 compliant.
  *
  * @see [[https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki BIP44]]
  */
sealed trait WalletApi {

  implicit val walletConfig: WalletAppConfig
  implicit val ec: ExecutionContext

  val nodeApi: NodeApi
  val chainQueryApi: ChainQueryApi

  def chainParams: ChainParams = walletConfig.chain

  def networkParameters: NetworkParameters = walletConfig.network
}

/**
  * API for a locked wallet
  */
trait LockedWalletApi extends WalletApi {

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
      blockHash: Option[DoubleSha256DigestBE]): Future[LockedWalletApi]

  /**
    * Processes the give block, updating our DB state if it's relevant to us.
    * @param block The block we're processing
    */
  def processBlock(block: Block): Future[LockedWalletApi]

  def processCompactFilter(
      blockHash: DoubleSha256Digest,
      blockFilter: GolombFilter): Future[LockedWalletApi] = {
    for {
      utxos <- listUtxos()
      addresses <- listAddresses()
      scriptPubKeys = utxos.flatMap(_.redeemScriptOpt).toSet ++ addresses
        .map(_.scriptPubKey)
        .toSet
      _ <- Future {
        val matcher = SimpleFilterMatcher(blockFilter)
        if (matcher.matchesAny(scriptPubKeys.toVector.map(_.asmBytes))) {
          nodeApi.downloadBlocks(Vector(blockHash))
        }
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

  /** Gets the sum of all confirmed UTXOs in this wallet */
  def getConfirmedBalance(): Future[CurrencyUnit]

  /** Gets the sum of all unconfirmed UTXOs in this wallet */
  def getUnconfirmedBalance(): Future[CurrencyUnit]

  /**
    * If a UTXO is spent outside of the wallet, we
    * need to remove it from the database so it won't be
    * attempted spent again by us.
    */
  // def updateUtxo: Future[WalletApi]

  /** Lists unspent transaction outputs in the wallet
    * @return Vector[SpendingInfoDb]
    * */
  def listUtxos(): Future[Vector[SpendingInfoDb]]

  def listAddresses(): Future[Vector[AddressDb]]

  /**
    * Gets a new external address with the specified
    * type. Calling this method multiple
    * times will return the same address, until it has
    * received funds.
    *  TODO: Last sentence is not true, implement that
    *  https://github.com/bitcoin-s/bitcoin-s/issues/628
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
    * Mimics the `getaddressinfo` RPC call in Bitcoin Core
    *
    * @param address
    * @return If the address is found in our database `Some(address)`
    *         is returned, otherwise `None`
    */
  def getAddressInfo(address: BitcoinAddress): Future[Option[AddressInfo]]

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
    * */
  protected[wallet] def getDefaultAccountForType(
      addressType: AddressType): Future[AccountDb]

  /**
    * Unlocks the wallet with the provided passphrase,
    * making it possible to send transactions.
    */
  def unlock(passphrase: AesPassword): Either[
    KeyManagerUnlockError,
    UnlockedWalletApi] = {
    val kmParams = walletConfig.kmParams

    val unlockedKeyManagerE =
      LockedKeyManager.unlock(passphrase, kmParams)
    unlockedKeyManagerE match {
      case Right(km) =>
        val w = Wallet(keyManager = km,
                       nodeApi = nodeApi,
                       chainQueryApi = chainQueryApi)
        Right(w)
      case Left(err) => Left(err)
    }
  }

  def listAccounts(): Future[Vector[AccountDb]]

  /** Lists all wallet accounts with the given type
    * @param purpose
    * @return [[Future[Vector[AccountDb]]
    * */
  def listAccounts(purpose: HDPurpose): Future[Vector[AccountDb]] =
    listAccounts().map(_.filter(_.hdAccount.purpose == purpose))

}

trait UnlockedWalletApi extends LockedWalletApi {

  def keyManager: KeyManager

  /**
    * Locks the wallet. After this operation is called,
    * all sensitive material in the wallet should be
    * encrypted and unaccessible
    */
  def lock(): LockedWalletApi

  /**
    *
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
    * Sends money from the default account
    *
    * todo: add error handling to signature
    */
  def sendToAddress(
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRate: FeeUnit
  ): Future[Transaction] = {
    for {
      account <- getDefaultAccount()
      tx <- sendToAddress(address, amount, feeRate, account)
    } yield tx
  }

  /**
    * Tries to create a new account in this wallet. Fails if the
    * most recent account has no transaction history, as per
    * BIP44
    *
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki#account BIP44 account section]]
    */
  def createNewAccount(keyManagerParams: KeyManagerParams): Future[WalletApi]

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
      parallelismLevel: Int = Runtime.getRuntime.availableProcessors())(
      implicit ec: ExecutionContext): Future[Vector[DoubleSha256DigestBE]] = {
    require(batchSize > 0, "batch size must be greater than zero")
    require(parallelismLevel > 0, "parallelism level must be greater than zero")

    if (scripts.isEmpty) {
      Future.successful(Vector.empty)
    } else {
      val bytes = scripts.map(_.asmBytes)

      /** Calculates group size to split a filter vector into [[parallelismLevel]] groups.
        * It's needed to limit number of threads required to run the matching */
      def calcGroupSize(vectorSize: Int): Int =
        if (vectorSize / parallelismLevel * parallelismLevel < vectorSize)
          vectorSize / parallelismLevel + 1
        else vectorSize / parallelismLevel

      def findMatches(
          filters: Vector[(GolombFilter, DoubleSha256DigestBE)]): Future[
        Iterator[DoubleSha256DigestBE]] = {
        if (filters.isEmpty)
          Future.successful(Iterator.empty)
        else {
          /* Iterates over the grouped vector of filters to find matches with the given [[bytes]]. */
          val groupSize = calcGroupSize(filters.size)
          val filterGroups = filters.grouped(groupSize)
          // Sequence on the filter groups making sure the number of threads doesn't exceed [[parallelismLevel]].
          Future
            .sequence(filterGroups.map { filterGroup =>
              // We need to wrap in a future here to make sure we can
              // potentially run these matches in parallel
              Future {
                // Find any matches in the group and add the corresponding block hashes into the result
                filterGroup.foldLeft(Vector.empty[DoubleSha256DigestBE]) {
                  (blocks, filter) =>
                    val matcher = new SimpleFilterMatcher(filter._1)
                    if (matcher.matchesAny(bytes)) {
                      blocks :+ filter._2
                    } else {
                      blocks
                    }
                }
              }
            })
            .map(_.flatten)
        }
      }

      /** Iterates over all filters in the range to find matches */
      @tailrec
      def loop(
          start: Int,
          end: Int,
          acc: Future[Vector[DoubleSha256DigestBE]]): Future[
        Vector[DoubleSha256DigestBE]] = {
        if (end <= start) {
          acc
        } else {
          val startHeight = end - (batchSize - 1)
          val endHeight = end
          val newAcc = for {
            compactFilterDbs <- chainQueryApi.getFiltersBetweenHeights(
              startHeight,
              endHeight)
            filtered <- findMatches(compactFilterDbs)
            res <- acc
          } yield {
            res ++ filtered
          }
          val newEnd = Math.max(start, endHeight - batchSize)
          loop(start, newEnd, newAcc)
        }
      }

      for {
        startHeight <- startOpt.fold(Future.successful(0))(
          chainQueryApi.getHeightByBlockStamp)
        _ = if (startHeight < 0)
          throw InvalidBlockRange(s"Start position cannot negative")
        endHeight <- endOpt.fold(chainQueryApi.getFilterCount)(
          chainQueryApi.getHeightByBlockStamp)
        _ = if (startHeight > endHeight)
          throw InvalidBlockRange(
            s"End position cannot precede start: $startHeight:$endHeight")
        matched <- loop(startHeight, endHeight, Future.successful(Vector.empty))
      } yield {
        matched
      }
    }
  }

  def rescan(
      scriptPubKeys: Vector[ScriptPubKey],
      startOpt: Option[BlockStamp] = None,
      endOpt: Option[BlockStamp] = None): Future[Unit] = {
    val threadPool =
      Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors() * 2)

    val res = for {
      blockHashes <- getMatchingBlocks(scriptPubKeys, startOpt, endOpt)(
        ExecutionContext.fromExecutor(threadPool))
      res <- nodeApi.downloadBlocks(blockHashes.map(_.flip))
    } yield {
      res
    }

    res.onComplete(_ => threadPool.shutdown())

//    res.failed.foreach(logger.error("Cannot rescan", _))

    res

  }

}
