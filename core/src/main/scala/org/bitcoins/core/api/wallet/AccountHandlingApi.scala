package org.bitcoins.core.api.wallet

import org.bitcoins.core.api.wallet.db.{AccountDb, AddressDb}
import org.bitcoins.core.crypto.ExtPublicKey
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.hd.{AddressType, HDAccount, HDChainType, HDPurpose}
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.ScriptPubKey

import scala.concurrent.{ExecutionContext, Future}

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

  /** Gets the balance of the given account */
  def getBalance(account: HDAccount)(implicit
      ec: ExecutionContext): Future[CurrencyUnit] = {
    val confirmedF = getConfirmedBalance(account)
    val unconfirmedF = getUnconfirmedBalance(account)
    for {
      confirmed <- confirmedF
      unconfirmed <- unconfirmedF
    } yield {
      confirmed + unconfirmed
    }
  }

  def getConfirmedBalance(account: HDAccount): Future[CurrencyUnit]

  def getUnconfirmedBalance(account: HDAccount): Future[CurrencyUnit]

  def getDefaultAccount(): Future[AccountDb]
  def listAccounts(): Future[Vector[AccountDb]]
  def listAccounts(purpose: HDPurpose)(implicit
      ec: ExecutionContext): Future[Vector[AccountDb]] = {
    listAccounts().map(_.filter(_.hdAccount.purpose == purpose))
  }
  def getDefaultAccountForType(addressType: AddressType): Future[AccountDb]
  def getNewAddress(
      account: AccountDb,
      chainType: HDChainType
  ): Future[BitcoinAddress]

  final def getNewAddress(account: HDAccount)(implicit
      ec: ExecutionContext): Future[BitcoinAddress] = {
    val accountDbOptF = findAccount(account)
    accountDbOptF.flatMap {
      case Some(accountDb) => getNewAddress(accountDb)
      case None =>
        Future.failed(
          new RuntimeException(
            s"No account found for given hdaccount=${account}"
          )
        )
    }
  }
  def getNewAddress(accountDb: AccountDb): Future[BitcoinAddress]

  final def getNewChangeAddress(account: HDAccount)(implicit
      ec: ExecutionContext): Future[BitcoinAddress] = {
    val accountDbOptF = findAccount(account)
    accountDbOptF.flatMap {
      case Some(accountDb) => getNewChangeAddress(accountDb)
      case None =>
        Future.failed(
          new RuntimeException(
            s"No account found for given hdaccount=${account}"
          )
        )
    }
  }
  def getNewChangeAddress(accountDb: AccountDb): Future[BitcoinAddress]
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
