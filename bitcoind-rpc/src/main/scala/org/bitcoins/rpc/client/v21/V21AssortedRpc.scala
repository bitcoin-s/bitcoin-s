package org.bitcoins.rpc.client.v21

import org.bitcoins.commons.jsonmodels.bitcoind.GetWalletInfoResultPostV21
import org.bitcoins.commons.serializers.JsonSerializers._
import org.bitcoins.rpc.client.common.{Client, WalletRpc}
import org.bitcoins.rpc.client.v18.V18AssortedRpc
import org.bitcoins.rpc.client.v20.V20AssortedRpc
import play.api.libs.json.Json

import scala.concurrent.Future

trait V21AssortedRpc extends V18AssortedRpc with V20AssortedRpc with WalletRpc {
  self: Client =>

  private def getWalletInfo(
      walletName: Option[String]): Future[GetWalletInfoResultPostV21] = {
    bitcoindCall[GetWalletInfoResultPostV21]("getwalletinfo",
                                             List(Json.toJson(walletName)))
  }

  override def getWalletInfo: Future[GetWalletInfoResultPostV21] = {
    getWalletInfo(None)
  }

  override def getWalletInfo(
      walletName: String): Future[GetWalletInfoResultPostV21] =
    getWalletInfo(Some(walletName))

}
