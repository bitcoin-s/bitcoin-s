package org.bitcoins.rpc.client.v25

import org.apache.pekko.actor.ActorSystem
import org.bitcoins.rpc.client.common.{BitcoindRpcClient, BitcoindVersion}
import org.bitcoins.rpc.config.BitcoindInstance

import scala.concurrent.Future
import scala.util.Try

class BitcoindV25RpcClient(override val instance: BitcoindInstance)(implicit
    actorSystem: ActorSystem
) extends BitcoindRpcClient(instance) {

  override lazy val version: Future[BitcoindVersion] =
    Future.successful(BitcoindVersion.V24)
}

object BitcoindV25RpcClient {

  /** Creates an RPC client from the given instance.
    *
    * Behind the scenes, we create an actor system for you. You can use
    * `withActorSystem` if you want to manually specify an actor system for the
    * RPC client.
    */
  def apply(instance: BitcoindInstance): BitcoindV25RpcClient = {
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
  ): BitcoindV25RpcClient =
    new BitcoindV25RpcClient(instance)(system)

  def fromUnknownVersion(
      rpcClient: BitcoindRpcClient
  ): Try[BitcoindV25RpcClient] =
    Try {
      new BitcoindV25RpcClient(rpcClient.instance)(rpcClient.system)
    }

}
