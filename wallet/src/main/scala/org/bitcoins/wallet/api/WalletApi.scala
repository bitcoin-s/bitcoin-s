package org.bitcoins.wallet.api

import org.bitcoins.core.api.{ChainQueryApi, NodeApi}
import org.bitcoins.core.bloom.BloomFilter
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.crypto._
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.gcs.{GolombFilter, SimpleFilterMatcher}
import org.bitcoins.core.hd.{AddressType, HDPurpose}
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.blockchain.{Block, ChainParams}
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.wallet.HDUtil
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.models.{AccountDb, AddressDb, SpendingInfoDb}

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
    * @param confirmations How many confirmations the TX has
    */
  def processTransaction(
      transaction: Transaction,
      confirmations: Int): Future[LockedWalletApi]

  /**
    * Processes the give block, updating our DB state if it's relevant to us.
    * @param block The block we're processing
    */
  def processBlock(block: Block, confirmations: Int): Future[LockedWalletApi]

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
  def unlock(passphrase: AesPassword): UnlockWalletResult

  def listAccounts(): Future[Vector[AccountDb]]

  /** Lists all wallet accounts with the given type
    * @param purpose
    * @return [[Future[Vector[AccountDb]]
    * */
  def listAccounts(purpose: HDPurpose): Future[Vector[AccountDb]] =
    listAccounts().map(_.filter(_.hdAccount.purpose == purpose))

}

trait UnlockedWalletApi extends LockedWalletApi {

  protected def mnemonicCode: MnemonicCode

  /** The wallet seed */
  protected lazy val seed: BIP39Seed = BIP39Seed.fromMnemonic(mnemonicCode)

  // TODO: come back to how to handle this
  def passphrase: AesPassword

  /** Derives the relevant xpriv for the given HD purpose */
  private[wallet] def xprivForPurpose(purpose: HDPurpose): ExtPrivateKey = {
    val seed = BIP39Seed.fromMnemonic(mnemonicCode, BIP39Seed.EMPTY_PASSWORD) // todo think more about this

    val privVersion = HDUtil.getXprivVersion(purpose)
    seed.toExtPrivateKey(privVersion)
  }

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
  def createNewAccount(purpose: HDPurpose): Future[WalletApi]

  /**
    * Tries to create a new account in this wallet for the default
    * account type. Fails if the
    * most recent account has no transaction history, as per
    * BIP44
    *
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki#account BIP44 account section]]
    */
  def createNewAccount(): Future[WalletApi]

}
