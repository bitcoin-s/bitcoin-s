package org.bitcoins.rpc.client.v18

import org.bitcoins.commons.jsonmodels.bitcoind.{
  GetRpcInfoResult,
  ListWalletDirResult
}
import org.bitcoins.commons.serializers.JsonSerializers._
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.rpc.client.common.{Client}
import play.api.libs.json.{JsString}

import scala.concurrent.Future

/** Assorted Rpc calls for Bitcoin V18
  * @see
  *   [[https://bitcoincore.org/en/doc/0.18.0/rpc/network/getnodeaddresses/]]
  * @see
  *   [[https://bitcoincore.org/en/doc/0.18.0/rpc/wallet/listwalletdir/]]
  * @see
  *   [[https://github.com/bitcoin/bitcoin/commit/e82f6ad6f270f1f101d8853be32fd11eff4ddfb8]]
  * @see
  *   [[https://bitcoincore.org/en/doc/0.18.0/rpc/mining/submitheader/]]
  */
trait V18AssortedRpc {
  self: Client =>

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
