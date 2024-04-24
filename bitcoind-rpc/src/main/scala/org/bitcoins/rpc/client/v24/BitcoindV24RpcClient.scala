package org.bitcoins.rpc.client.v24

import org.apache.pekko.actor.ActorSystem
import org.bitcoins.commons.jsonmodels.bitcoind.SimulateRawTransactionResult
import org.bitcoins.commons.serializers.JsonSerializers._
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.rpc.client.common.{BitcoindRpcClient, BitcoindVersion}
import org.bitcoins.rpc.config.BitcoindInstance
import play.api.libs.json._

import scala.concurrent.Future
import scala.util.Try

/** Class for creating a BitcoindV24 instance that can access RPCs
  */
class BitcoindV24RpcClient(override val instance: BitcoindInstance)(implicit
    actorSystem: ActorSystem
) extends BitcoindRpcClient(instance) {

  override lazy val version: Future[BitcoindVersion] =
    Future.successful(BitcoindVersion.V24)
}

object BitcoindV24RpcClient {

  /** Creates an RPC client from the given instance.
    *
    * Behind the scenes, we create an actor system for you. You can use
    * `withActorSystem` if you want to manually specify an actor system for the
    * RPC client.
    */
  def apply(instance: BitcoindInstance): BitcoindV24RpcClient = {
    implicit val system: ActorSystem =
      ActorSystem.create(BitcoindRpcClient.ActorSystemName)
    withActorSystem(instance)
  }

  /** Creates an RPC client from the given instance, together with the given
    * actor system. This is for advanced users, where you need fine grained
    * control over the RPC client.
    */
  def withActorSystem(instance: BitcoindInstance)(implicit
      system: ActorSystem
  ): BitcoindV24RpcClient =
    new BitcoindV24RpcClient(instance)(system)

  def fromUnknownVersion(
      rpcClient: BitcoindRpcClient
  ): Try[BitcoindV24RpcClient] =
    Try {
      new BitcoindV24RpcClient(rpcClient.instance)(rpcClient.system)
    }

}
