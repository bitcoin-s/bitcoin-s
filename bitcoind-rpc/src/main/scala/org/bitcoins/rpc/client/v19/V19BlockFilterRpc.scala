package org.bitcoins.rpc.client.v19

import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.core.gcs.{BlockFilter, FilterType}
import org.bitcoins.rpc.client.common.Client
import org.bitcoins.rpc.jsonmodels.GetBlockFilterResult
import org.bitcoins.rpc.serializers.JsonReaders.DoubleSha256DigestBEReads
import play.api.libs.json._
import play.api.libs.functional.syntax._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

/**
  * Gets the BIP158 filter for the specified block.
  * This RPC is only enabled if block filters have been created using the -blockfilterindex configuration option
  * @see [[https://bitcoincore.org/en/doc/0.19.0/rpc/blockchain/getblockfilter]]
  */
trait V19BlockFilterRpc {
  self: Client =>

  private case class TempBlockFilterResult(
      filter: String,
      header: DoubleSha256DigestBE)
  implicit private val tempBlockFilterResultReads: Reads[
    TempBlockFilterResult] =
    ((__ \ "filter").read[String] and
      (__ \ "header").read[DoubleSha256DigestBE])(TempBlockFilterResult)

  def getBlockFilter(
      blockhash: DoubleSha256DigestBE,
      filtertype: FilterType): Future[GetBlockFilterResult] = {
    val temp = Await.result(bitcoindCall[TempBlockFilterResult](
                              "getblockfilter",
                              List(JsString(blockhash.hex),
                                   JsString(filtertype.toString.toLowerCase))),
                            Duration.Inf)
    Future {
      GetBlockFilterResult(BlockFilter.fromHex(temp.filter, blockhash.flip),
                           temp.header)
    }
  }

}
