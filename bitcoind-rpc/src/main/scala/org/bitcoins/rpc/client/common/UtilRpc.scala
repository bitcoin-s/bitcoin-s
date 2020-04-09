package org.bitcoins.rpc.client.common

import org.bitcoins.commons.jsonmodels.bitcoind.{
  DecodeScriptResult,
  ValidateAddressResult,
  ValidateAddressResultImpl
}
import org.bitcoins.commons.serializers.JsonSerializers._
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.ScriptPubKey
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
}
