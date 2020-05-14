package org.bitcoins.rpc.client.v19

import akka.actor.ActorSystem
import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.WalletFlag
import org.bitcoins.commons.jsonmodels.bitcoind.{
  GetBalancesResult,
  RpcOpts,
  SetWalletFlagResult,
  SignRawTransactionResult
}
import org.bitcoins.commons.serializers.JsonSerializers._
import org.bitcoins.commons.serializers.JsonWriters._
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.crypto.ECPrivateKey
import org.bitcoins.rpc.client.common.{
  BitcoindRpcClient,
  BitcoindVersion,
  DescriptorRpc,
  PsbtRpc
}
import org.bitcoins.rpc.config.BitcoindInstance
import play.api.libs.json._

import scala.concurrent.Future
import scala.util.Try

/**
  * Class for creating a BitcoindV19 instance that can access RPCs
  */
class BitcoindV19RpcClient(override val instance: BitcoindInstance)(
    implicit
    actorSystem: ActorSystem)
    extends BitcoindRpcClient(instance)
    with DescriptorRpc
    with PsbtRpc
    with V19BlockFilterRpc {

  override lazy val version: BitcoindVersion = BitcoindVersion.V19

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

  /**
    * Change the state of the given wallet flag for a wallet.
    */
  def setWalletFlag(
      flag: WalletFlag,
      value: Boolean
  ): Future[SetWalletFlagResult] =
    bitcoindCall[SetWalletFlagResult](
      "setwalletflag",
      List(JsString(flag.toString), Json.toJson(value)))

  def getBalances: Future[GetBalancesResult] = {
    bitcoindCall[GetBalancesResult]("getbalances")
  }

}

object BitcoindV19RpcClient {

  /**
    * Creates an RPC client from the given instance.
    *
    * Behind the scenes, we create an actor system for
    * you. You can use `withActorSystem` if you want to
    * manually specify an actor system for the RPC client.
    */
  def apply(instance: BitcoindInstance): BitcoindV19RpcClient = {
    implicit val system =
      ActorSystem.create(BitcoindRpcClient.ActorSystemName)
    withActorSystem(instance)
  }

  /**
    * Creates an RPC client from the given instance,
    * together with the given actor system. This is for
    * advanced users, where you need fine grained control
    * over the RPC client.
    */
  def withActorSystem(instance: BitcoindInstance)(
      implicit system: ActorSystem): BitcoindV19RpcClient =
    new BitcoindV19RpcClient(instance)(system)

  def fromUnknownVersion(
      rpcClient: BitcoindRpcClient): Try[BitcoindV19RpcClient] =
    Try {
      new BitcoindV19RpcClient(rpcClient.instance)(rpcClient.system)
    }

}
