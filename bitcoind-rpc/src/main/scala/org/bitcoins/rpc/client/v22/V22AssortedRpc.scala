package org.bitcoins.rpc.client.v22

import org.bitcoins.commons.jsonmodels.bitcoind.GetNodeAddressesResultPostV22
import org.bitcoins.commons.serializers.JsonSerializers._
import org.bitcoins.rpc.client.common.Client
import org.bitcoins.rpc.client.v18.V18AssortedRpc
import org.bitcoins.rpc.client.v20.V20AssortedRpc
import play.api.libs.json.Json

import scala.concurrent.Future

trait V22AssortedRpc extends V18AssortedRpc with V20AssortedRpc{ self:Client =>
  private def getNodeAddresses(
                                count: Option[Int]): Future[Vector[GetNodeAddressesResultPostV22]] = {
    bitcoindCall[Vector[GetNodeAddressesResultPostV22]]("getnodeaddresses",
      List(Json.toJson(count)))
}

  override def getNodeAddresses(count: Int): Future[Vector[GetNodeAddressesResultPostV22]] =
    getNodeAddresses(Some(count))

  override def getNodeAddresses(): Future[Vector[GetNodeAddressesResultPostV22]] =
    getNodeAddresses(None)

}
