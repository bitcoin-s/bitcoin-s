package org.bitcoins.rpc.client.v19

import org.bitcoins.commons.jsonmodels.bitcoind.GetBlockFilterResult
import org.bitcoins.commons.serializers.JsonReaders.DoubleSha256DigestBEReads
import org.bitcoins.core.gcs.{BlockFilter, FilterType}
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.rpc.client.common.Client
import play.api.libs.json._

import scala.concurrent.Future

/** Gets the BIP158 filter for the specified block.
  * This RPC is only enabled if block filters have been created using the -blockfilterindex configuration option
  * @see [[https://bitcoincore.org/en/doc/0.19.0/rpc/blockchain/getblockfilter]]
  */
trait V19BlockFilterRpc {
  self: Client =>

  /** This is needed because we need the block hash to create a GolombFilter.
    * We use an intermediary data type to hold our data so we can add the block hash
    * we were given after the RPC call
    */
  private case class TempBlockFilterResult(
      filter: String,
      header: DoubleSha256DigestBE)

  implicit
  private val tempBlockFilterResultReads: Reads[TempBlockFilterResult] =
    Json.reads[TempBlockFilterResult]

  def getBlockFilter(
      blockhash: DoubleSha256DigestBE,
      filtertype: FilterType): Future[GetBlockFilterResult] = {
    bitcoindCall[TempBlockFilterResult](
      "getblockfilter",
      List(JsString(blockhash.hex), JsString(filtertype.toString.toLowerCase)))
      .map { tempBlockFilterResult =>
        GetBlockFilterResult(
          BlockFilter.fromHex(tempBlockFilterResult.filter, blockhash.flip),
          tempBlockFilterResult.header)
      }
  }

}
