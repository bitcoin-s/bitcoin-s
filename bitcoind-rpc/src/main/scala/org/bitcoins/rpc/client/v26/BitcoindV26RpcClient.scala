package org.bitcoins.rpc.client.v26

import org.apache.pekko.actor.ActorSystem
import org.bitcoins.rpc.client.common.{BitcoindRpcClient, BitcoindVersion}
import org.bitcoins.rpc.config.{BitcoindInstance, BitcoindRpcAppConfig}

import scala.concurrent.Future

class BitcoindV26RpcClient(override val instance: BitcoindInstance)(implicit
    actorSystem: ActorSystem,
    bitcoindRpcAppConfig: BitcoindRpcAppConfig
) extends BitcoindRpcClient(instance) {

  override lazy val version: Future[BitcoindVersion] =
    Future.successful(BitcoindVersion.V26)
}

object BitcoindV26RpcClient {

  /** Creates an RPC client from the given instance, together with the given
    * actor system. This is for advanced users, where you need fine grained
    * control over the RPC client.
    */
  def apply(instance: BitcoindInstance)(implicit
      system: ActorSystem,
      bitcoindRpcAppConfig: BitcoindRpcAppConfig
  ): BitcoindV26RpcClient =
    new BitcoindV26RpcClient(instance)(system, bitcoindRpcAppConfig)

}
