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
import org.bitcoins.rpc.jsonmodels.{
  AddressInfoResult,
  CreateWalletResult,
  SignRawTransactionResult,
  TestMempoolAcceptResult
}
import org.bitcoins.rpc.serializers.JsonSerializers._
import org.bitcoins.rpc.serializers.JsonWriters._
import play.api.libs.json.{JsArray, JsBoolean, JsString, Json}

import scala.concurrent.Future
import scala.util.Try

/**
  * This class is compatible with version 0.17 of Bitcoin Core.
  *
  * @see [[org.bitcoins.rpc.client.common.BitcoindRpcClient BitcoindRpcClient Scaladocs]]
  *
  * @define signRawTx Bitcoin Core 0.17 had a breaking change in the API
  *                   for signing raw transactions. Previously the same
  *                   RPC call was used for signing a TX with existing keys
  *                   in the Bitcoin Core wallet or a manually provided private key.
  *                   These RPC calls are now separated out into two distinct calls.
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

  /**
    * $signRawTx
    *
    * This RPC call signs the raw transaction with keys found in
    * the Bitcoin Core wallet.
    */
  def signRawTransactionWithWallet(
      transaction: Transaction,
      utxoDeps: Vector[RpcOpts.SignRawTransactionOutputParameter] = Vector.empty,
      sigHash: HashType = HashType.sigHashAll
  ): Future[SignRawTransactionResult] =
    bitcoindCall[SignRawTransactionResult]("signrawtransactionwithwallet",
                                           List(JsString(transaction.hex),
                                                Json.toJson(utxoDeps),
                                                Json.toJson(sigHash)))

  /**
    * $signRawTx
    *
    * This RPC call signs the raw transaction with keys provided
    * manually.
    */
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

  // testmempoolaccept expects (and returns) a list of txes,
  // but currently only lists of length 1 is supported
  def testMempoolAccept(
      transaction: Transaction,
      allowHighFees: Boolean = false): Future[TestMempoolAcceptResult] = {
    bitcoindCall[Vector[TestMempoolAcceptResult]](
      "testmempoolaccept",
      List(JsArray(Vector(Json.toJson(transaction))), JsBoolean(allowHighFees)))
      .map(_.head)
  }

  def createWallet(
      walletName: String,
      disablePrivateKeys: Boolean = false): Future[CreateWalletResult] = {
    bitcoindCall[CreateWalletResult](
      "createwallet",
      List(JsString(walletName), Json.toJson(disablePrivateKeys)))
  }
}

object BitcoindV17RpcClient {

  def fromUnknownVersion(rpcClient: BitcoindRpcClient)(
      implicit actorSystem: ActorSystem): Try[BitcoindV17RpcClient] =
    Try {
      new BitcoindV17RpcClient(rpcClient.instance)
    }
}
