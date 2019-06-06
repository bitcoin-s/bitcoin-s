package org.bitcoins.wallet.api

import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.crypto._
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.hd.HDPurpose
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.blockchain.ChainParams
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.wallet.HDUtil
import org.bitcoins.wallet.models.{AccountDb, AddressDb, UTXOSpendingInfoDb}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.core.bloom.BloomFilter

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
    * Adds the provided UTXO to the wallet, making it
    * available for spending.
    */
  def addUtxo(transaction: Transaction, vout: UInt32): Future[AddUtxoResult]

  /** Sums up the value of all UTXOs in the wallet */
  // noinspection AccessorLikeMethodIsEmptyParen
  // async calls have side effects :-)
  def getBalance(): Future[CurrencyUnit]

  /**
    * If a UTXO is spent outside of the wallet, we
    * need to remove it from the database so it won't be
    * attempted spent again by us.
    */
  // def updateUtxo: Future[WalletApi]

  def listUtxos(): Future[Vector[UTXOSpendingInfoDb]]

  def listAddresses(): Future[Vector[AddressDb]]

  /**
    * Gets a new external address from the specified
    * account. Calling this method multiple
    * times will return the same address, until it has
    * received funds.
    */
  def getNewAddress(account: AccountDb): Future[BitcoinAddress]

  /**
    * Gets a new external address from the default account.
    * Calling this method multiple
    * times will return the same address, until it has
    * received funds.
    */
  def getNewAddress(): Future[BitcoinAddress] = {
    for {
      account <- getDefaultAccount()
      address <- getNewAddress(account)
    } yield address
  }

  /**
    * Mimics the `getaddressinfo` RPC call in Bitcoin Core
    *
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
    */
  protected[wallet] def getDefaultAccount(): Future[AccountDb]

  /**
    * Unlocks the wallet with the provided passphrase,
    * making it possible to send transactions.
    */
  def unlock(passphrase: AesPassword): UnlockWalletResult

  def listAccounts(): Future[Vector[AccountDb]]

  /**
    * Tries to create a new accoun in this wallet. Fails if the
    * most recent account has no transaction history, as per
    * BIP44
    *
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki#account BIP44 account section]]
    */
  // def createNewAccount: Future[Try[WalletApi]]

}

trait UnlockedWalletApi extends LockedWalletApi {

  def mnemonicCode: MnemonicCode

  /** The wallet seed */
  lazy val seed: BIP39Seed = BIP39Seed.fromMnemonic(mnemonicCode)

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

}
