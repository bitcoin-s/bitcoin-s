package org.bitcoins.rpc.client.v29

import org.apache.pekko.actor.ActorSystem
import org.bitcoins.rpc.client.common.{BitcoindRpcClient, BitcoindVersion}
import org.bitcoins.rpc.config.{BitcoindInstance, BitcoindInstanceLocal}

import scala.concurrent.Future

class BitcoindV29RpcClient(override val instance: BitcoindInstance)(implicit
    actorSystem: ActorSystem
) extends BitcoindRpcClient(instance) {
  override lazy val version: Future[BitcoindVersion] =
    Future.successful(BitcoindVersion.V29)
}

object BitcoindV29RpcClient {

  /** Creates an RPC client from the given instance, together with the given
    * actor system. This is for advanced users, where you need fine grained
    * control over the RPC client.
    */
  def apply(instance: BitcoindInstanceLocal)(implicit
      system: ActorSystem
  ): BitcoindV29RpcClient =
    new BitcoindV29RpcClient(instance)(using system)

}
