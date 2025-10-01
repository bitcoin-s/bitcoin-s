package org.bitcoins.rpc.client.v30

import org.apache.pekko.actor.ActorSystem
import org.bitcoins.rpc.client.common.{BitcoindRpcClient, BitcoindVersion}
import org.bitcoins.rpc.config.{BitcoindInstance, BitcoindInstanceLocal}

import scala.concurrent.Future

class BitcoindV30RpcClient(override val instance: BitcoindInstance)(implicit
    actorSystem: ActorSystem
) extends BitcoindRpcClient(instance) {
  override lazy val version: Future[BitcoindVersion] =
    Future.successful(BitcoindVersion.V30)
}

object BitcoindV30RpcClient {

  /** Creates an RPC client from the given instance, together with the given
    * actor system. This is for advanced users, where you need fine grained
    * control over the RPC client.
    */
  def apply(instance: BitcoindInstanceLocal)(implicit
      system: ActorSystem
  ): BitcoindV30RpcClient =
    new BitcoindV30RpcClient(instance)(system)

}
