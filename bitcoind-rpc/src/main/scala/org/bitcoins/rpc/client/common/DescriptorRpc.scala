package org.bitcoins.rpc.client.common

import org.bitcoins.commons.jsonmodels.bitcoind.{
  DeriveAddressesResult,
  GetDescriptorInfoResult
}
import org.bitcoins.commons.serializers.JsonSerializers._
import play.api.libs.json.{JsString, Json}

import scala.concurrent.Future

/** RPC calls in V18 that use descriptor to give us output information
  * @see [[https://bitcoincore.org/en/doc/0.18.0/rpc/util/deriveaddresses/]]
  * @see [[https://bitcoincore.org/en/doc/0.18.0/rpc/util/getdescriptorinfo/]]
  */
trait DescriptorRpc {
  self: Client =>

  def deriveAddresses(
      descriptor: String,
      range: Option[Vector[Double]]): Future[DeriveAddressesResult] = {
    val params =
      if (range.isDefined) List(JsString(descriptor), Json.toJson(range))
      else List(JsString(descriptor))
    bitcoindCall[DeriveAddressesResult]("deriveaddresses", params)
  }

  def getDescriptorInfo(descriptor: String): Future[GetDescriptorInfoResult] = {
    bitcoindCall[GetDescriptorInfoResult]("getdescriptorinfo",
                                          List(JsString(descriptor)))
  }
}
