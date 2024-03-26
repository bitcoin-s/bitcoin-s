package org.bitcoins.rpc.client.v21

import org.apache.pekko.actor.ActorSystem
import org.bitcoins.rpc.client.common.{
  BitcoindRpcClient,
  BitcoindVersion,
  DescriptorRpc,
  PsbtRpc
}
import org.bitcoins.rpc.client.v20.{V20AssortedRpc, V20MultisigRpc}
import org.bitcoins.rpc.config.BitcoindInstance

import scala.concurrent.Future
import scala.util.Try

/** Class for creating a BitcoindV21 instance that can access RPCs
  */
class BitcoindV21RpcClient(override val instance: BitcoindInstance)(implicit
    actorSystem: ActorSystem)
    extends BitcoindRpcClient(instance)
    with DescriptorRpc
    with PsbtRpc
    with V20MultisigRpc
    with V20AssortedRpc {

  override lazy val version: Future[BitcoindVersion] =
    Future.successful(BitcoindVersion.V21)
}

object BitcoindV21RpcClient {

  /** Creates an RPC client from the given instance.
    *
    * Behind the scenes, we create an actor system for
    * you. You can use `withActorSystem` if you want to
    * manually specify an actor system for the RPC client.
    */
  def apply(instance: BitcoindInstance): BitcoindV21RpcClient = {
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
      system: ActorSystem): BitcoindV21RpcClient =
    new BitcoindV21RpcClient(instance)(system)

  def fromUnknownVersion(
      rpcClient: BitcoindRpcClient): Try[BitcoindV21RpcClient] =
    Try {
      new BitcoindV21RpcClient(rpcClient.instance)(rpcClient.system)
    }

}
