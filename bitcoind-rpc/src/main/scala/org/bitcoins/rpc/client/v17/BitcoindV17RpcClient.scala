package org.bitcoins.rpc.client.v17

import akka.actor.ActorSystem
import org.bitcoins.core.crypto.ECPrivateKey
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.rpc.client.common.{
  BitcoindRpcClient,
  BitcoindVersion,
  RpcOpts
}
import org.bitcoins.rpc.config.BitcoindInstance
import org.bitcoins.rpc.jsonmodels.{AddressInfoResult, SignRawTransactionResult}
import org.bitcoins.rpc.serializers.JsonSerializers._
import org.bitcoins.rpc.serializers.JsonWriters._
import play.api.libs.json.{JsString, Json}

import scala.concurrent.Future
import scala.util.Try

/**
  * This class is compatible with version 0.17 of Bitcoin Core.
  */
class BitcoindV17RpcClient(override val instance: BitcoindInstance)(
    implicit
    actorSystem: ActorSystem)
    extends BitcoindRpcClient(instance)
    with V17LabelRpc
    with V17PsbtRpc {

  override def version: BitcoindVersion = BitcoindVersion.V17

  def getAddressInfo(address: BitcoinAddress): Future[AddressInfoResult] = {
    bitcoindCall[AddressInfoResult]("getaddressinfo",
                                    List(JsString(address.value)))
  }

  def signRawTransactionWithWallet(
      transaction: Transaction,
      utxoDeps: Vector[RpcOpts.SignRawTransactionOutputParameter] = Vector.empty,
      sigHash: HashType = HashType.sigHashAll
  ): Future[SignRawTransactionResult] =
    bitcoindCall[SignRawTransactionResult]("signrawtransactionwithwallet",
                                           List(JsString(transaction.hex),
                                                Json.toJson(utxoDeps),
                                                Json.toJson(sigHash)))

  def signRawTransactionWithKey(
      transaction: Transaction,
      keys: Vector[ECPrivateKey],
      utxoDeps: Vector[RpcOpts.SignRawTransactionOutputParameter] = Vector.empty,
      sigHash: HashType = HashType.sigHashAll
  ): Future[SignRawTransactionResult] =
    bitcoindCall[SignRawTransactionResult]("signrawtransactionwithkey",
                                           List(JsString(transaction.hex),
                                                Json.toJson(keys),
                                                Json.toJson(utxoDeps),
                                                Json.toJson(sigHash)))

}

object BitcoindV17RpcClient {

  def fromUnknownVersion(rpcClient: BitcoindRpcClient)(
      implicit actorSystem: ActorSystem): Try[BitcoindV17RpcClient] =
    Try {
      new BitcoindV17RpcClient(rpcClient.instance)
    }
}
