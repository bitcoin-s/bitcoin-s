package org.bitcoins.rpc.client.v19

import akka.actor.ActorSystem
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.rpc.client.common.{BitcoindRpcClient, BitcoindVersion}
import org.bitcoins.rpc.config.BitcoindInstance
import org.bitcoins.rpc.jsonmodels.{GetBalancesResult, SetWalletFlagResult}
import play.api.libs.json.Json
import play.api.libs.json.JsString
import org.bitcoins.rpc.serializers.JsonSerializers._

import scala.concurrent.Future
import scala.util.Try

/**
  * Class for creating a BitcoindV19 instance that can access RPCs
  * @param instance
  * @param actorSystem
  */
class BitcoindV19RpcClient(override val instance: BitcoindInstance)(
    implicit
    actorSystem: ActorSystem)
    extends BitcoindRpcClient(instance)
    with V19BlockFilterRpc {

  override lazy val version: BitcoindVersion = BitcoindVersion.V19

  /**
    * setWalletFlag
    *
    * Change the state of the given wallet flag for a wallet.
    */
  def setWalletFlag(
      flag: String,
      value: Boolean
  ): Future[SetWalletFlagResult] =
    bitcoindCall[SetWalletFlagResult]("setwalletflag",
                                      List(JsString(flag), Json.toJson(value)))

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
