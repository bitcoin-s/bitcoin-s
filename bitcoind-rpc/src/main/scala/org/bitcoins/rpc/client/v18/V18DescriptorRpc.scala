package org.bitcoins.rpc.client.v18
import org.bitcoins.rpc.client.common.Client
import org.bitcoins.rpc.jsonmodels.{
  DeriveAddressesResult,
  GetDescriptorInfoResult
}
import play.api.libs.json.{JsString, Json}
import org.bitcoins.rpc.serializers.JsonSerializers._

import scala.concurrent.Future

/**
  * RPC calls in V18 that use descriptor to give us output information
  * @see [[https://bitcoincore.org/en/doc/0.18.0/rpc/util/deriveaddresses/]]
  * @see [[https://bitcoincore.org/en/doc/0.18.0/rpc/util/getdescriptorinfo/]]
  */
trait V18DescriptorRpc {
  self: Client =>

  def deriveAddresses(
      descriptor: String,
      range: Option[Vector[Double]]): Future[DeriveAddressesResult] = {
    bitcoindCall[DeriveAddressesResult](
      "deriveaddresses",
      List(JsString(descriptor), Json.toJson(range))) //todo: specify call arguments
  }

  def getDescriptorInfo(descriptor: String): Future[GetDescriptorInfoResult] = {
    bitcoindCall[GetDescriptorInfoResult]("getdescriptorinfo",
                                          List(JsString(descriptor)))
  }
}
