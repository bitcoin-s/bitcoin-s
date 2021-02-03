package org.bitcoins.rpc.client.v16

import org.bitcoins.commons.jsonmodels.bitcoind.ReceivedAccount
import org.bitcoins.commons.serializers.JsonReaders._
import org.bitcoins.commons.serializers.JsonSerializers._
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.rpc.client.common.Client
import play.api.libs.json.{JsBoolean, JsNumber, JsString}

import scala.concurrent.Future

/** Bitcoin Core prior to version 0.17 had the concept of
  * accounts. This has later been removed, and replaced
  * with a label system, as well as functionality for
  * having several distinct wallets active at the same time.
  */
trait V16AccountRpc { self: Client =>

  def getAccountAddress(account: String): Future[BitcoinAddress] = {
    bitcoindCall[BitcoinAddress]("getaccountaddress", List(JsString(account)))
  }

  def getReceivedByAccount(
      account: String,
      confirmations: Int = 1): Future[Bitcoins] = {
    bitcoindCall[Bitcoins]("getreceivedbyaccount",
                           List(JsString(account), JsNumber(confirmations)))
  }

  def getAccount(address: BitcoinAddress): Future[String] = {
    bitcoindCall[String]("getaccount", List(JsString(address.value)))
  }

  def getAddressesByAccount(account: String): Future[Vector[BitcoinAddress]] = {
    bitcoindCall[Vector[BitcoinAddress]]("getaddressesbyaccount",
                                         List(JsString(account)))
  }

  def listAccounts(
      confirmations: Int = 1,
      includeWatchOnly: Boolean = false): Future[Map[String, Bitcoins]] = {
    bitcoindCall[Map[String, Bitcoins]](
      "listaccounts",
      List(JsNumber(confirmations), JsBoolean(includeWatchOnly)))
  }

  def setAccount(address: BitcoinAddress, account: String): Future[Unit] = {
    bitcoindCall[Unit]("setaccount",
                       List(JsString(address.value), JsString(account)))
  }

  def listReceivedByAccount(
      confirmations: Int = 1,
      includeEmpty: Boolean = false,
      includeWatchOnly: Boolean = false): Future[Vector[ReceivedAccount]] = {
    bitcoindCall[Vector[ReceivedAccount]]("listreceivedbyaccount",
                                          List(JsNumber(confirmations),
                                               JsBoolean(includeEmpty),
                                               JsBoolean(includeWatchOnly)))
  }
}
