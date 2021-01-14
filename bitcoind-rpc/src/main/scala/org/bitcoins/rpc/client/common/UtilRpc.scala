package org.bitcoins.rpc.client.common

import org.bitcoins.commons.jsonmodels.bitcoind._
import org.bitcoins.commons.serializers.JsonSerializers._
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.rpc.client.common.BitcoindVersion._
import play.api.libs.json.{JsString, Json}

import scala.concurrent.Future

/*
 * Utility RPC calls
 */
trait UtilRpc { self: Client =>

  def validateAddress(
      address: BitcoinAddress): Future[ValidateAddressResult] = {
    bitcoindCall[ValidateAddressResultImpl]("validateaddress",
                                            List(JsString(address.toString)))
  }

  def decodeScript(script: ScriptPubKey): Future[DecodeScriptResult] = {
    bitcoindCall[DecodeScriptResult]("decodescript", List(Json.toJson(script)))
  }

  def getIndexInfo: Future[Map[String, IndexInfoResult]] = {
    version match {
      case V21 | Unknown =>
        bitcoindCall[Map[String, IndexInfoResult]]("getindexinfo")
      case V16 | V17 | V18 | V19 | V20 | Experimental =>
        Future.failed(
          new RuntimeException(
            s"getIndexInfo is only for version V21+, got $version"))
    }
  }

  def getIndexInfo(indexName: String): Future[IndexInfoResult] = {
    version match {
      case V21 | Unknown =>
        bitcoindCall[Map[String, IndexInfoResult]](
          "getindexinfo",
          List(JsString(indexName))).map(_.head._2)
      case V16 | V17 | V18 | V19 | V20 | Experimental =>
        Future.failed(
          new RuntimeException(
            s"getIndexInfo is only for version V21+, got $version"))
    }

  }
}
