package org.bitcoins.rpc.client.v19

import org.bitcoins.rpc.client.common.Client
import org.bitcoins.rpc.jsonmodels.GetBlockFilterResult
import org.bitcoins.rpc.serializers.JsonSerializers._
import play.api.libs.json._

import scala.concurrent.Future

/**
  * Gets the BIP158 filter for the specified block.
  * This RPC is only enabled if block filters have been created using the -blockfilterindex configuration option
  * @see [[https://bitcoincore.org/en/doc/0.19.0/rpc/blockchain/getblockfilter]]
  */
trait V19BlockFilterRpc {
  self: Client =>

  def getBlockFilter(
      blockhash: String,
      filtertype: String): Future[GetBlockFilterResult] = {
    bitcoindCall[GetBlockFilterResult](
      "getblockfilter",
      List(JsString(blockhash), JsString(filtertype)))
  }

}
