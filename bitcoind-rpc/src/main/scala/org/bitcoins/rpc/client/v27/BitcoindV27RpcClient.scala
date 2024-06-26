package org.bitcoins.rpc.client.v27

import org.apache.pekko.actor.ActorSystem
import org.bitcoins.rpc.client.common.{BitcoindRpcClient, BitcoindVersion}
import org.bitcoins.rpc.config.BitcoindInstance

import scala.concurrent.Future
import scala.util.Try

class BitcoindV27RpcClient(override val instance: BitcoindInstance)(implicit
    actorSystem: ActorSystem
) extends BitcoindRpcClient(instance) {

  override lazy val version: Future[BitcoindVersion] =
    Future.successful(BitcoindVersion.V27)
}

object BitcoindV27RpcClient {

  /** Creates an RPC client from the given instance.
    *
    * Behind the scenes, we create an actor system for you. You can use
    * `withActorSystem` if you want to manually specify an actor system for the
    * RPC client.
    */
  def apply(instance: BitcoindInstance): BitcoindV27RpcClient = {
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
  ): BitcoindV27RpcClient =
    new BitcoindV27RpcClient(instance)(system)

  def fromUnknownVersion(
      rpcClient: BitcoindRpcClient
  ): Try[BitcoindV27RpcClient] =
    Try {
      new BitcoindV27RpcClient(rpcClient.instance)(rpcClient.system)
    }

}
