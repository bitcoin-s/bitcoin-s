package org.bitcoins.rpc.client.v27

import org.apache.pekko.actor.ActorSystem
import org.bitcoins.rpc.client.common.{BitcoindRpcClient, BitcoindVersion}
import org.bitcoins.rpc.config.{BitcoindInstance, BitcoindRpcAppConfig}

import scala.concurrent.Future

class BitcoindV27RpcClient(override val instance: BitcoindInstance)(implicit
    actorSystem: ActorSystem,
    bitcoindRpcAppConfig: BitcoindRpcAppConfig
) extends BitcoindRpcClient(instance) {

  override lazy val version: Future[BitcoindVersion] =
    Future.successful(BitcoindVersion.V27)
}

object BitcoindV27RpcClient {

  /** Creates an RPC client from the given instance, together with the given
    * actor system. This is for advanced users, where you need fine grained
    * control over the RPC client.
    */
  def apply(instance: BitcoindInstance)(implicit
      system: ActorSystem,
      bitcoindRpcAppConfig: BitcoindRpcAppConfig
  ): BitcoindV27RpcClient =
    new BitcoindV27RpcClient(instance)(system, bitcoindRpcAppConfig)

}
