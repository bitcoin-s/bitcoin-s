package org.bitcoins.rpc.client

import akka.stream.ActorMaterializer
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.rpc.client.RpcOpts.LabelPurpose
import org.bitcoins.rpc.config.BitcoindInstance
import org.bitcoins.rpc.jsonmodels.{ AddressInfoResult, AddressesByLabelResult, ReceivedLabel }
import org.bitcoins.rpc.serializers.BitcoindJsonSerializers._
import play.api.libs.json.{ JsBoolean, JsNumber, JsString }

import scala.concurrent.Future

class BitcoindV17RpcClient(override protected val instance: BitcoindInstance)(
  implicit
  m: ActorMaterializer) extends BitcoindRpcClient(instance) {

  override val version: BitcoindVersion = BitcoindV17

  def getAddressInfo(address: BitcoinAddress): Future[AddressInfoResult] = {
    bitcoindCall[AddressInfoResult]("getaddressinfo", List(JsString(address.value)))
  }

  def getAddressesByLabel(label: String): Future[Map[BitcoinAddress, AddressesByLabelResult]] = {
    bitcoindCall[Map[BitcoinAddress, AddressesByLabelResult]]("getaddressesbylabel", List(JsString(label)))
  }

  def getReceivedByLabel(
    account: String,
    confirmations: Int = 1): Future[Bitcoins] = {
    bitcoindCall[Bitcoins](
      "getreceivedbylabel",
      List(JsString(account), JsNumber(confirmations)))
  }

  def listLabels(
    purpose: Option[LabelPurpose] = None): Future[Vector[String]] = {
    bitcoindCall[Vector[String]](
      "listlabels",
      List(JsString(purpose match {
        case None => ""
        case Some(p) => p.toString
      })))
  }

  def listReceivedByLabel(
    confirmations: Int = 1,
    includeEmpty: Boolean = false,
    includeWatchOnly: Boolean = false): Future[Vector[ReceivedLabel]] = {
    bitcoindCall[Vector[ReceivedLabel]](
      "listreceivedbyaccount",
      List(
        JsNumber(confirmations),
        JsBoolean(includeEmpty),
        JsBoolean(includeWatchOnly)))
  }
}

