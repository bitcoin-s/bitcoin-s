package org.bitcoins.wallet.api

import org.bitcoins.core.crypto.bip44.BIP44Account
import org.bitcoins.core.crypto.{ExtPublicKey, MnemonicCode}
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.{Address, BitcoinAddress}
import org.bitcoins.core.protocol.blockchain.ChainParams
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.wallet.utxo.UTXOSpendingInfo
import org.bitcoins.db.DbConfig

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
  def addUtxo(utxo: UTXOSpendingInfo): Future[WalletApi]

  /**
    * If a UTXO is spent outside of the wallet, we
    * need to remove it from the database so it won't be
    * attempted spent again by us.
    */
  def updateUtxo: Future[WalletApi]

  /**
    * Gets a new external address. Calling this method multiple
    * times will return the same address, until it has
    * received funds.
    */
  def getNewAddress: Future[Address]

  /**
    * Unlocks the wallet with the provided passphrase,
    * making it possible to send transactions.
    */
  def unlock(passphrase: String): Future[Try[UnlockedWalletApi]]

  /**
    * Every wallet has at least one
    * [[org.bitcoins.core.crypto.bip44.BIP44Account BIP44Account]]
    */
  def getAccounts: Future[Vector[BIP44Account]]

  /**
    * Tries to create a new accoun in this wallet. Fails if the
    * most recent account has no transaction history, as per
    * BIP44
    *
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki#account BIP44 account section]]
    */
  def createNewAccount: Future[Try[WalletApi]]
}

trait UnlockedWalletApi extends WalletApi with LockedWalletApi {

  def mnemonicCode: Future[MnemonicCode]

  /**
    * Locks the wallet. After this operation is called,
    * all sensitive material in the wallet should be
    * encrypted and unaccessible
    */
  def lock: Future[WalletApi]
}
