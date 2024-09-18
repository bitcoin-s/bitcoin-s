package org.bitcoins.core.api.wallet

import org.bitcoins.core.api.wallet.db.AccountDb
import org.bitcoins.core.hd.{AddressType, HDAccount}
import org.bitcoins.core.protocol.script.ScriptPubKey

import scala.concurrent.Future

trait AccountHandlingApi {
  def getDefaultAccount(): Future[AccountDb]
  def listAccounts(): Future[Vector[AccountDb]]
  def getDefaultAccountForType(addressType: AddressType): Future[AccountDb]
  def clearUtxos(account: HDAccount): Future[Unit]
  def generateScriptPubKeys(
      account: HDAccount,
      addressBatchSize: Int,
      forceGenerateSpks: Boolean
  ): Future[Vector[ScriptPubKey]]
}
