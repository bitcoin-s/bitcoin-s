package org.bitcoins.rpc.client.common

import org.bitcoins.core.crypto.ECPublicKey
import org.bitcoins.core.protocol.P2PKHAddress
import org.bitcoins.rpc.client.common.RpcOpts.AddressType
import org.bitcoins.rpc.jsonmodels.MultiSigResult
import org.bitcoins.rpc.serializers.BitcoindJsonSerializers._
import play.api.libs.json.{ JsArray, JsNumber, JsString, Json }

import scala.concurrent.Future

trait MultisigRpc extends Client {

  private def addMultiSigAddress(
    minSignatures: Int,
    keys: Vector[Either[ECPublicKey, P2PKHAddress]],
    account: String = "",
    addressType: Option[AddressType]): Future[MultiSigResult] = {
    def keyToString(key: Either[ECPublicKey, P2PKHAddress]): JsString =
      key match {
        case Right(k) => JsString(k.value)
        case Left(k) => JsString(k.hex)
      }

    val params =
      if (addressType.isEmpty) {
        List(
          JsNumber(minSignatures),
          JsArray(keys.map(keyToString)),
          JsString(account))
      } else {
        List(
          JsNumber(minSignatures),
          JsArray(keys.map(keyToString)),
          JsString(account),
          JsString(addressType
            .map(_.toString)
            .getOrElse("")))
      }

    bitcoindCall[MultiSigResult]("addmultisigaddress", params)
  }

  def addMultiSigAddress(
    minSignatures: Int,
    keys: Vector[Either[ECPublicKey, P2PKHAddress]]): Future[MultiSigResult] =
    addMultiSigAddress(minSignatures, keys, addressType = None)

  def addMultiSigAddress(
    minSignatures: Int,
    keys: Vector[Either[ECPublicKey, P2PKHAddress]],
    account: String): Future[MultiSigResult] =
    addMultiSigAddress(minSignatures, keys, account, None)

  def addMultiSigAddress(
    minSignatures: Int,
    keys: Vector[Either[ECPublicKey, P2PKHAddress]],
    addressType: AddressType): Future[MultiSigResult] =
    addMultiSigAddress(minSignatures, keys, addressType = Some(addressType))

  def addMultiSigAddress(
    minSignatures: Int,
    keys: Vector[Either[ECPublicKey, P2PKHAddress]],
    account: String,
    addressType: AddressType): Future[MultiSigResult] =
    addMultiSigAddress(minSignatures, keys, account, Some(addressType))

  def createMultiSig(
    minSignatures: Int,
    keys: Vector[ECPublicKey]): Future[MultiSigResult] = {
    bitcoindCall[MultiSigResult](
      "createmultisig",
      List(JsNumber(minSignatures), Json.toJson(keys.map(_.hex))))
  }

}
