package org.bitcoins.rpc.client.v20

import org.bitcoins.commons.jsonmodels.bitcoind.MultiSigResultPostV20
import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.AddressType
import org.bitcoins.commons.serializers.JsonSerializers._
import org.bitcoins.commons.serializers.JsonWriters._
import org.bitcoins.core.protocol.P2PKHAddress
import org.bitcoins.crypto.ECPublicKey
import org.bitcoins.rpc.client.common.{BitcoindRpcClient, Client, MultisigRpc}
import play.api.libs.json.{JsArray, JsNumber, JsString, Json}

import scala.concurrent.Future

/** This trait defines RPC calls related to multisignature functionality in
  * Bitcoin Core.
  *
  * @see
  *   [[https://en.bitcoin.it/wiki/Multisignature Bitcoin Wiki]] article on
  *   multisignature.
  */
trait V20MultisigRpc extends MultisigRpc { self: Client =>

  private def addMultiSigAddress(
      minSignatures: Int,
      keys: Vector[Either[ECPublicKey, P2PKHAddress]],
      account: String = "",
      addressType: Option[AddressType]
  ): Future[MultiSigResultPostV20] = {
    def keyToString(key: Either[ECPublicKey, P2PKHAddress]): JsString =
      key match {
        case Right(k) => JsString(k.value)
        case Left(k)  => JsString(k.hex)
      }

    val params =
      List(
        JsNumber(minSignatures),
        JsArray(keys.map(keyToString)),
        JsString(account)
      ) ++ addressType.map(Json.toJson(_)).toList

    bitcoindCall[MultiSigResultPostV20]("addmultisigaddress", params)
  }

  override def addMultiSigAddress(
      minSignatures: Int,
      keys: Vector[Either[ECPublicKey, P2PKHAddress]]
  ): Future[MultiSigResultPostV20] =
    addMultiSigAddress(minSignatures, keys, addressType = None)

  override def addMultiSigAddress(
      minSignatures: Int,
      keys: Vector[Either[ECPublicKey, P2PKHAddress]],
      account: String
  ): Future[MultiSigResultPostV20] =
    addMultiSigAddress(minSignatures, keys, account, None)

  override def addMultiSigAddress(
      minSignatures: Int,
      keys: Vector[Either[ECPublicKey, P2PKHAddress]],
      addressType: AddressType
  ): Future[MultiSigResultPostV20] =
    addMultiSigAddress(minSignatures, keys, addressType = Some(addressType))

  override def addMultiSigAddress(
      minSignatures: Int,
      keys: Vector[Either[ECPublicKey, P2PKHAddress]],
      account: String,
      addressType: AddressType
  ): Future[MultiSigResultPostV20] =
    addMultiSigAddress(minSignatures, keys, account, Some(addressType))

  override def createMultiSig(
      minSignatures: Int,
      keys: Vector[ECPublicKey],
      addressType: AddressType,
      walletName: String = BitcoindRpcClient.DEFAULT_WALLET_NAME
  ): Future[MultiSigResultPostV20] = {
    bitcoindCall[MultiSigResultPostV20](
      "createmultisig",
      List(
        JsNumber(minSignatures),
        Json.toJson(keys.map(_.hex)),
        Json.toJson(addressType)
      ),
      uriExtensionOpt = Some(walletExtension(walletName))
    )
  }
}
