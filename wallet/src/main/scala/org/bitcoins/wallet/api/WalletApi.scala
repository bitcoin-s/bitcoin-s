package org.bitcoins.wallet.api

import org.bitcoins.core.config.{BitcoinNetwork, MainNet, RegTest, TestNet3}
import org.bitcoins.core.crypto.{
  BIP39Seed,
  ExtKeyPrivVersion,
  ExtPrivateKey,
  MnemonicCode
}
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.blockchain.{
  ChainParams,
  MainNetChainParams,
  RegTestNetChainParams,
  TestNetChainParams
}
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.db.DbConfig
import org.bitcoins.wallet.models.UTXOSpendingInfoDb

import scala.concurrent.Future
import scala.util.Try

/**
  * API for the wallet project.
  *
  * This wallet API is BIP344 compliant.
  *
  * @see [[https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki BIP44]]
  */
sealed trait WalletApi {

  def dbConfig: DbConfig

  def chainParams: ChainParams

  def networkParameters: BitcoinNetwork = chainParams match {
    case MainNetChainParams    => MainNet
    case TestNetChainParams    => TestNet3
    case RegTestNetChainParams => RegTest
  }

  def feeProvider: FeeProvider
}

/**
  * API for a locked wallet
  */
trait LockedWalletApi extends WalletApi {

  /**
    * Adds the provided UTXO to the wallet, making it
    * available for spending.
    */
  def addUtxo(
      transaction: Transaction,
      vout: UInt32): Future[Either[AddUtxoError, WalletApi]]

  /**
    * If a UTXO is spent outside of the wallet, we
    * need to remove it from the database so it won't be
    * attempted spent again by us.
    */
  // def updateUtxo: Future[WalletApi]

  def listUtxos(): Future[Vector[UTXOSpendingInfoDb]]

  /**
    * Gets a new external address. Calling this method multiple
    * times will return the same address, until it has
    * received funds.
    */
  def getNewAddress(accountIndex: Int = 0): Future[BitcoinAddress]

  /**
    * Unlocks the wallet with the provided passphrase,
    * making it possible to send transactions.
    */
  def unlock(passphrase: String): Future[Try[UnlockedWalletApi]]

  /**
    * Every wallet has at least one
    * [[org.bitcoins.core.crypto.bip44.BIP44Account BIP44Account]]
    */
  // def getAccounts: Future[Vector[BIP44Account]]

  /**
    * Tries to create a new accoun in this wallet. Fails if the
    * most recent account has no transaction history, as per
    * BIP44
    *
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki#account BIP44 account section]]
    */
  // def createNewAccount: Future[Try[WalletApi]]

}

trait UnlockedWalletApi extends WalletApi with LockedWalletApi {

  def mnemonicCode: MnemonicCode

  // todo better type?
  def passphrase: String

  def xpriv: ExtPrivateKey = {
    val seed = BIP39Seed.fromMnemonic(mnemonicCode, passphrase)

    val privVersion = ExtKeyPrivVersion.fromNetworkParameters(networkParameters)
    seed.toExtPrivateKey(privVersion)
  }

  /**
    * Locks the wallet. After this operation is called,
    * all sensitive material in the wallet should be
    * encrypted and unaccessible
    */
  def lock: Future[WalletApi]

  /**
    * todo: add error handling to signature
    *
    * @param address The recipient of the TX
    * @param amount the amount being sent
    * @param accountIndex the account to send money from. Defaults to 0;
    */
  def sendToAddress(
      address: BitcoinAddress,
      amount: CurrencyUnit,
      accountIndex: Int = 0): Future[Transaction]
}
