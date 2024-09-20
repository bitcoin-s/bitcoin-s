package org.bitcoins.core.api.wallet

import org.bitcoins.core.api.wallet.db.{AccountDb, AddressDb}
import org.bitcoins.core.crypto.ExtPublicKey
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.hd.{AddressType, HDAccount, HDChainType, HDPurpose}
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.ScriptPubKey

import scala.concurrent.Future

trait AccountHandlingApi {

  /** Tries to create a new account in this wallet. Fails if the most recent
    * account has no transaction history, as per BIP44
    *
    * @see
    *   [[https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki#account BIP44 account section]]
    */
  def createNewAccount(
      hdAccount: HDAccount
  ): Future[ExtPublicKey]

  def createNewAccount(purpose: HDPurpose): Future[ExtPublicKey]

  def getDefaultAccount(): Future[AccountDb]
  def listAccounts(): Future[Vector[AccountDb]]
  def getDefaultAccountForType(addressType: AddressType): Future[AccountDb]
  def getNewAddress(
      account: AccountDb,
      chainType: HDChainType
  ): Future[BitcoinAddress]

  def getNewAddress(account: HDAccount): Future[BitcoinAddress]
  final def getNewAddress(accountDb: AccountDb): Future[BitcoinAddress] = {
    getNewAddress(accountDb.hdAccount)
  }
  def getNewChangeAddress(account: HDAccount): Future[BitcoinAddress]
  final def getNewChangeAddress(
      accountDb: AccountDb): Future[BitcoinAddress] = {
    getNewChangeAddress(accountDb.hdAccount)
  }
  def clearUtxos(account: HDAccount): Future[Unit]
  def findAccount(account: HDAccount): Future[Option[AccountDb]]
  def generateScriptPubKeys(
      account: HDAccount,
      addressBatchSize: Int,
      forceGenerateSpks: Boolean
  ): Future[Vector[ScriptPubKey]]
  def listUnusedAddresses(
      account: HDAccount
  ): Future[Vector[AddressDb]]
  def listSpentAddresses(
      account: HDAccount
  ): Future[Vector[AddressDb]]
  def listAddresses(account: HDAccount): Future[Vector[AddressDb]]
  def listFundedAddresses(
      account: HDAccount
  ): Future[Vector[(AddressDb, CurrencyUnit)]]
}
