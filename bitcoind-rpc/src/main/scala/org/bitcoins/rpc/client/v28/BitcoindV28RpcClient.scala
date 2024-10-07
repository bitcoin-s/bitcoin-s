package org.bitcoins.rpc.client.v28

import org.apache.pekko.actor.ActorSystem
import org.bitcoins.rpc.client.common.{BitcoindRpcClient, BitcoindVersion}
import org.bitcoins.rpc.config.{
  BitcoindInstance,
  BitcoindInstanceLocal,
  BitcoindRpcAppConfig
}

import scala.concurrent.Future

class BitcoindV28RpcClient(override val instance: BitcoindInstance)(implicit
    actorSystem: ActorSystem,
    bitcoindRpcAppConfig: BitcoindRpcAppConfig
) extends BitcoindRpcClient(instance) {

  override lazy val version: Future[BitcoindVersion] =
    Future.successful(BitcoindVersion.V28)
}

object BitcoindV28RpcClient {

  /** Creates an RPC client from the given instance, together with the given
    * actor system. This is for advanced users, where you need fine grained
    * control over the RPC client.
    */
  def apply(instance: BitcoindInstanceLocal)(implicit
      system: ActorSystem
  ): BitcoindV28RpcClient =
    new BitcoindV28RpcClient(instance)(system, instance.bitcoindRpcAppConfig)

}
