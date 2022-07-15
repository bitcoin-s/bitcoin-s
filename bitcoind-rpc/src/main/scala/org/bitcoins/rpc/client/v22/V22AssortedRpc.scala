package org.bitcoins.rpc.client.v22

import org.bitcoins.commons.jsonmodels.bitcoind.{
  listDescriptorsResult,
  GetNodeAddressesResultPostV22
}
import org.bitcoins.commons.serializers.JsonSerializers._
import org.bitcoins.rpc.client.common.Client
import org.bitcoins.rpc.client.v18.V18AssortedRpc
import org.bitcoins.rpc.client.v20.V20AssortedRpc
import play.api.libs.json.Json

import scala.concurrent.Future

trait V22AssortedRpc extends V18AssortedRpc with V20AssortedRpc {
  self: Client =>

  def listDescriptors(): Future[listDescriptorsResult] = {
    bitcoindCall[listDescriptorsResult](
      "listdescriptors"
    )
  }

  /**  def listDescriptors(Private: Option[Boolean],
    *                      walletName: Option[String] = None): Future[Vector[listDescriptorsResult]] = {
    *    bitcoindCall[Vector[listDescriptorsResult]](
    *      "listdescriptors", List(Json.toJson(Private)),
    *      uriExtensionOpt = walletName.map(walletExtension)
    *    )
    *  }
    */

  def listDescriptors(
      Private: Option[Boolean],
      walletName: String): Future[listDescriptorsResult] = {
    bitcoindCall[listDescriptorsResult](
      "listdescriptors",
      List(Json.toJson(Private)),
      uriExtensionOpt = Some(walletExtension(walletName))
    )
  }

  def listDescriptors(
      Private: Option[Boolean]): Future[listDescriptorsResult] = {
    bitcoindCall[listDescriptorsResult](
      "listdescriptors",
      List(Json.toJson(Private))
    )
  }

  def listDescriptors(walletName: String): Future[listDescriptorsResult] = {
    bitcoindCall[listDescriptorsResult](
      "listdescriptors",
      uriExtensionOpt = Some(walletExtension(walletName))
    )
  }

  private def getNodeAddresses(
      count: Option[Int]): Future[Vector[GetNodeAddressesResultPostV22]] = {
    bitcoindCall[Vector[GetNodeAddressesResultPostV22]](
      "getnodeaddresses",
      List(Json.toJson(count)))
  }

  def getNodeAddresses(
      network: String,
      count: Int): Future[Vector[GetNodeAddressesResultPostV22]] = {
    bitcoindCall[Vector[GetNodeAddressesResultPostV22]](
      "getnodeaddresses",
      List(Json.toJson(count), Json.toJson(network))
    )
  }

  override def getNodeAddresses(
      count: Int): Future[Vector[GetNodeAddressesResultPostV22]] =
    getNodeAddresses(Some(count))

  override def getNodeAddresses(): Future[
    Vector[GetNodeAddressesResultPostV22]] =
    getNodeAddresses(None)

  /**  def testMempoolAccept(
    *      transaction: Transaction,
    *      allowHighFees: Boolean = false): Future[
    *    TestMempoolAcceptResultPostV22] = {
    *    bitcoindCall[Vector[TestMempoolAcceptResultPostV22]](
    *      "testmempoolaccept",
    *      List(JsArray(Vector(Json.toJson(transaction))), JsBoolean(allowHighFees)))
    *      .map(_.head)
    *  }
    */

}
