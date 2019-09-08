package org.bitcoins.rpc.client.v18
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.rpc.client.common.Client
import org.bitcoins.rpc.jsonmodels.{
  GetNodeAddressesResult,
  GetRpcInfoResult,
  ListWalletDirResult
}
import org.bitcoins.rpc.serializers.JsonSerializers._
import play.api.libs.json.{JsString, Json}

import scala.concurrent.Future

/**
  * Assorted Rpc calls for Bitcoin V18
  * @see [[https://bitcoincore.org/en/doc/0.18.0/rpc/network/getnodeaddresses/]]
  * @see [[https://bitcoincore.org/en/doc/0.18.0/rpc/wallet/listwalletdir/]]
  * @see [[https://github.com/bitcoin/bitcoin/commit/e82f6ad6f270f1f101d8853be32fd11eff4ddfb8]]
  * @see [[https://bitcoincore.org/en/doc/0.18.0/rpc/mining/submitheader/]]
  */
trait V18AssortedRpc {
  self: Client =>

  private def getNodeAddresses(
      count: Option[Int]): Future[Vector[GetNodeAddressesResult]] = {
    bitcoindCall[Vector[GetNodeAddressesResult]]("getnodeaddresses",
                                                 List(Json.toJson(count)))
  }

  def getNodeAddresses(count: Int): Future[Vector[GetNodeAddressesResult]] =
    getNodeAddresses(Some(count))

  def getNodeAddresses(): Future[Vector[GetNodeAddressesResult]] =
    getNodeAddresses(None)

  def listWalletDir(): Future[ListWalletDirResult] = {
    bitcoindCall[ListWalletDirResult]("listwalletdir")
  }

  def getRpcInfo(): Future[GetRpcInfoResult] = {
    bitcoindCall[GetRpcInfoResult]("getrpcinfo")
  }

  def submitHeader(header: BlockHeader): Future[Unit] = {
    bitcoindCall[Unit]("submitheader", List(JsString(header.hex)))
  }

}
