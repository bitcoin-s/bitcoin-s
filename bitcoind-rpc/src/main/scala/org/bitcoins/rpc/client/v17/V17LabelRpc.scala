package org.bitcoins.rpc.client.v17

import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.rpc.client.common.Client
import org.bitcoins.rpc.client.common.RpcOpts.LabelPurpose
import org.bitcoins.rpc.jsonmodels.{LabelResult, ReceivedLabel}
import org.bitcoins.rpc.serializers.JsonSerializers._
import play.api.libs.json.{JsBoolean, JsNumber, JsString}

import scala.concurrent.Future

/**
  * Bitcoin Core prior to version 0.17 had the concept of
  * accounts. This has later been removed, and replaced
  * with a label system, as well as functionality for
  * having several distinct wallets active at the same time.
  */
trait V17LabelRpc { self: Client =>

  def getAddressesByLabel(
      label: String): Future[Map[BitcoinAddress, LabelResult]] = {
    bitcoindCall[Map[BitcoinAddress, LabelResult]]("getaddressesbylabel",
                                                   List(JsString(label)))
  }

  def getReceivedByLabel(
      account: String,
      confirmations: Int = 1): Future[Bitcoins] = {
    bitcoindCall[Bitcoins]("getreceivedbylabel",
                           List(JsString(account), JsNumber(confirmations)))
  }

  def setLabel(address: BitcoinAddress, label: String): Future[Unit] = {
    bitcoindCall[Unit]("setlabel",
                       List(JsString(address.value), JsString(label)))
  }

  def listLabels(
      purpose: Option[LabelPurpose] = None): Future[Vector[String]] = {
    bitcoindCall[Vector[String]]("listlabels",
                                 List(JsString(purpose.getOrElse("").toString)))
  }

  def listReceivedByLabel(
      confirmations: Int = 1,
      includeEmpty: Boolean = false,
      includeWatchOnly: Boolean = false): Future[Vector[ReceivedLabel]] = {
    bitcoindCall[Vector[ReceivedLabel]]("listreceivedbylabel",
                                        List(JsNumber(confirmations),
                                             JsBoolean(includeEmpty),
                                             JsBoolean(includeWatchOnly)))
  }
}
