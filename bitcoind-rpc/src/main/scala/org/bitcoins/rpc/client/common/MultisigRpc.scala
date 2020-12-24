package org.bitcoins.rpc.client.common

import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.AddressType
import org.bitcoins.commons.jsonmodels.bitcoind.{
  MultiSigResult,
  MultiSigResultPostV20,
  MultiSigResultPreV20
}
import org.bitcoins.commons.serializers.JsonSerializers._
import org.bitcoins.commons.serializers.JsonWriters._
import org.bitcoins.core.protocol.P2PKHAddress
import org.bitcoins.crypto.ECPublicKey
import org.bitcoins.rpc.client.common.BitcoindVersion._
import play.api.libs.json.{JsArray, JsNumber, JsString, Json}

import scala.concurrent.Future

/**
  * This trait defines RPC calls related to
  * multisignature functionality in Bitcoin Core.
  *
  * @see [[https://en.bitcoin.it/wiki/Multisignature Bitcoin Wiki]]
  *     article on multisignature.
  */
trait MultisigRpc { self: Client =>

  private def addMultiSigAddress(
      minSignatures: Int,
      keys: Vector[Either[ECPublicKey, P2PKHAddress]],
      account: String = "",
      addressType: Option[AddressType],
      walletNameOpt: Option[String] = None): Future[MultiSigResult] = {
    def keyToString(key: Either[ECPublicKey, P2PKHAddress]): JsString =
      key match {
        case Right(k) => JsString(k.value)
        case Left(k)  => JsString(k.hex)
      }

    val params =
      List(JsNumber(minSignatures),
           JsArray(keys.map(keyToString)),
           JsString(account)) ++ addressType.map(Json.toJson(_)).toList

    self.version match {
      case V21 | V20 | Unknown =>
        bitcoindCall[MultiSigResultPostV20](
          "addmultisigaddress",
          params,
          uriExtensionOpt = walletNameOpt.map(walletExtension))
      case V16 | V17 | V18 | V19 | Experimental =>
        bitcoindCall[MultiSigResultPreV20]("addmultisigaddress",
                                           params,
                                           uriExtensionOpt =
                                             walletNameOpt.map(walletExtension))
    }
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
      keys: Vector[ECPublicKey],
      walletNameOpt: Option[String] = None): Future[MultiSigResult] = {
    self.version match {
      case V21 | V20 | Unknown =>
        bitcoindCall[MultiSigResultPostV20](
          "createmultisig",
          List(JsNumber(minSignatures), Json.toJson(keys.map(_.hex))),
          uriExtensionOpt = walletNameOpt.map(walletExtension))
      case V16 | V17 | V18 | V19 | Experimental =>
        bitcoindCall[MultiSigResultPreV20](
          "createmultisig",
          List(JsNumber(minSignatures), Json.toJson(keys.map(_.hex))),
          uriExtensionOpt = walletNameOpt.map(walletExtension))
    }
  }
}
