package org.bitcoins.rpc.client.v25

import org.apache.pekko.actor.ActorSystem
import org.bitcoins.rpc.client.common.{BitcoindRpcClient, BitcoindVersion}
import org.bitcoins.rpc.config.{
  BitcoindInstance,
  BitcoindInstanceLocal,
  BitcoindRpcAppConfig
}

import scala.concurrent.Future

class BitcoindV25RpcClient(override val instance: BitcoindInstance)(implicit
    actorSystem: ActorSystem,
    bitcoindRpcAppConfig: BitcoindRpcAppConfig
) extends BitcoindRpcClient(instance) {

  override lazy val version: Future[BitcoindVersion] =
    Future.successful(BitcoindVersion.V25)
}

object BitcoindV25RpcClient {

  /** Creates an RPC client from the given instance, together with the given
    * actor system. This is for advanced users, where you need fine grained
    * control over the RPC client.
    */
  def apply(instance: BitcoindInstanceLocal)(implicit
      system: ActorSystem
  ): BitcoindV25RpcClient =
    new BitcoindV25RpcClient(instance)(system, instance.bitcoindRpcAppConfig)

}
