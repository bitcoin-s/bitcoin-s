package org.bitcoins.rpc.client.v22

import akka.actor.ActorSystem
import org.bitcoins.rpc.client.common.{
  BitcoindRpcClient,
  BitcoindVersion,
  DescriptorRpc,
  PsbtRpc
}
import org.bitcoins.rpc.client.v19.V19BlockFilterRpc
import org.bitcoins.rpc.client.v20.V20MultisigRpc
import org.bitcoins.rpc.config.BitcoindInstance

import scala.concurrent.Future
import scala.util.Try

/** Class for creating a BitcoindV22 instance that can access RPCs
  */
class BitcoindV22RpcClient(override val instance: BitcoindInstance)(implicit
    actorSystem: ActorSystem)
    extends BitcoindRpcClient(instance)
    with DescriptorRpc
    with PsbtRpc
    with V19BlockFilterRpc
    with V20MultisigRpc
    with V22AssortedRpc {

  override lazy val version: Future[BitcoindVersion] =
    Future.successful(BitcoindVersion.V22)

}

object BitcoindV22RpcClient {

  /** Creates an RPC client from the given instance.
    *
    * Behind the scenes, we create an actor system for
    * you. You can use `withActorSystem` if you want to
    * manually specify an actor system for the RPC client.
    */
  def apply(instance: BitcoindInstance): BitcoindV22RpcClient = {
    implicit val system: ActorSystem =
      ActorSystem.create(BitcoindRpcClient.ActorSystemName)
    withActorSystem(instance)
  }

  /** Creates an RPC client from the given instance,
    * together with the given actor system. This is for
    * advanced users, where you need fine grained control
    * over the RPC client.
    */
  def withActorSystem(instance: BitcoindInstance)(implicit
      system: ActorSystem): BitcoindV22RpcClient =
    new BitcoindV22RpcClient(instance)(system)

  def fromUnknownVersion(
      rpcClient: BitcoindRpcClient): Try[BitcoindV22RpcClient] =
    Try {
      new BitcoindV22RpcClient(rpcClient.instance)(rpcClient.system)
    }

}
