package org.bitcoins.core.api.wallet

import org.bitcoins.core.api.wallet.db.AccountDb
import org.bitcoins.core.hd.AddressType

import scala.concurrent.Future

trait AccountHandlingApi {
  def getDefaultAccount(): Future[AccountDb]
  def listAccounts(): Future[Vector[AccountDb]]
  def getDefaultAccountForType(addressType: AddressType): Future[AccountDb]
}
