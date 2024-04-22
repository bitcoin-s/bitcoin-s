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
      address: BitcoinAddress
  ): Future[ValidateAddressResult] = {
    bitcoindCall[ValidateAddressResultImpl](
      "validateaddress",
      List(JsString(address.toString))
    )
  }

  def decodeScript(script: ScriptPubKey): Future[DecodeScriptResult] = {
    self.version.flatMap { case V24 | Unknown =>
      bitcoindCall[DecodeScriptResultV22](
        "decodescript",
        List(Json.toJson(script))
      )
    }

  }

  def getIndexInfo: Future[Map[String, IndexInfoResult]] = {
    version.flatMap { case V24 | Unknown =>
      bitcoindCall[Map[String, IndexInfoResult]]("getindexinfo")
    }
  }

  def getIndexInfo(indexName: String): Future[IndexInfoResult] = {
    version.flatMap { case V24 | Unknown =>
      bitcoindCall[Map[String, IndexInfoResult]](
        "getindexinfo",
        List(JsString(indexName))
      ).map(_.head._2)
    }

  }
}
