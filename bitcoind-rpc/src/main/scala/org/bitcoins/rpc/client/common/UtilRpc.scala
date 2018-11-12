package org.bitcoins.rpc.client.common

import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.util.BitcoinSUtil
import org.bitcoins.rpc.jsonmodels.{DecodeScriptResult, ValidateAddressResult}
import org.bitcoins.rpc.serializers.JsonSerializers._
import play.api.libs.json.JsString

import scala.concurrent.Future

/*
 * Utility RPC calls
 */
trait UtilRpc extends Client {

  def validateAddress(
      address: BitcoinAddress): Future[ValidateAddressResult] = {
    bitcoindCall[ValidateAddressResult]("validateaddress",
                                        List(JsString(address.toString)))
  }

  // TODO: add ScriptPubKey.asmHex
  def decodeScript(script: ScriptPubKey): Future[DecodeScriptResult] = {
    bitcoindCall[DecodeScriptResult](
      "decodescript",
      List(JsString(BitcoinSUtil.encodeHex(script.asmBytes))))
  }
}
