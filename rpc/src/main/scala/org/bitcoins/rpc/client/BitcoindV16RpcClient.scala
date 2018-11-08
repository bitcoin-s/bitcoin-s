package org.bitcoins.rpc.client

import akka.stream.ActorMaterializer
import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.rpc.config.BitcoindInstance
import org.bitcoins.rpc.jsonmodels.ReceivedAccount
import org.bitcoins.rpc.serializers.BitcoindJsonReaders._
import org.bitcoins.rpc.serializers.BitcoindJsonSerializers._
import play.api.libs.json.{ JsBoolean, JsNumber, JsString }

import scala.concurrent.Future

class BitcoindV16RpcClient(override protected val instance: BitcoindInstance)(
  implicit
  m: ActorMaterializer) extends BitcoindRpcClient(instance) {

  override val version: BitcoindVersion = BitcoindV16

  def move(
    fromAccount: String,
    toAccount: String,
    amount: Bitcoins,
    comment: String = ""): Future[Boolean] = {
    bitcoindCall[Boolean](
      "move",
      List(
        JsString(fromAccount),
        JsString(toAccount),
        JsNumber(amount.toBigDecimal),
        JsNumber(6),
        JsString(comment)))
  }

  def sendFrom(
    fromAccount: String,
    toAddress: BitcoinAddress,
    amount: Bitcoins,
    confirmations: Int = 1,
    comment: String = "",
    toComment: String = ""): Future[DoubleSha256Digest] = {
    bitcoindCall[DoubleSha256Digest](
      "sendfrom",
      List(
        JsString(fromAccount),
        JsString(toAddress.value),
        JsNumber(amount.toBigDecimal),
        JsNumber(confirmations),
        JsString(comment),
        JsString(toComment)))
  }

  def getAccountAddress(account: String): Future[BitcoinAddress] = {
    bitcoindCall[BitcoinAddress]("getaccountaddress", List(JsString(account)))
  }

  def getReceivedByAccount(
    account: String,
    confirmations: Int = 1): Future[Bitcoins] = {
    bitcoindCall[Bitcoins](
      "getreceivedbyaccount",
      List(JsString(account), JsNumber(confirmations)))
  }

  def getAccount(address: BitcoinAddress): Future[String] = {
    bitcoindCall[String]("getaccount", List(JsString(address.value)))
  }

  def getAddressesByAccount(account: String): Future[Vector[BitcoinAddress]] = {
    bitcoindCall[Vector[BitcoinAddress]](
      "getaddressesbyaccount",
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
    bitcoindCall[Unit](
      "setaccount",
      List(JsString(address.value), JsString(account)))
  }

  def listReceivedByAccount(
    confirmations: Int = 1,
    includeEmpty: Boolean = false,
    includeWatchOnly: Boolean = false): Future[Vector[ReceivedAccount]] = {
    bitcoindCall[Vector[ReceivedAccount]](
      "listreceivedbyaccount",
      List(
        JsNumber(confirmations),
        JsBoolean(includeEmpty),
        JsBoolean(includeWatchOnly)))
  }
}
