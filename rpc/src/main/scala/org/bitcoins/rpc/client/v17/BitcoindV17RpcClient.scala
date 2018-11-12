package org.bitcoins.rpc.client.v17

import akka.stream.ActorMaterializer
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.rpc.client.common.{ BitcoindRpcClient, BitcoindVersion }
import org.bitcoins.rpc.config.BitcoindInstance
import org.bitcoins.rpc.jsonmodels.AddressInfoResult
import org.bitcoins.rpc.serializers.BitcoindJsonSerializers._
import play.api.libs.json.JsString

import scala.concurrent.Future

/**
 * This class is compatible with version 0.17 of Bitcoin Core.
 */
class BitcoindV17RpcClient(override protected val instance: BitcoindInstance)(
  implicit
  m: ActorMaterializer) extends BitcoindRpcClient(instance) with V17LabelRpc {

  override val version: BitcoindVersion = BitcoindVersion.V17

  def getAddressInfo(address: BitcoinAddress): Future[AddressInfoResult] = {
    bitcoindCall[AddressInfoResult]("getaddressinfo", List(JsString(address.value)))
  }

}

