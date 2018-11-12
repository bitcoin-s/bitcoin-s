package org.bitcoins.rpc.client.v16

import akka.stream.ActorMaterializer
import org.bitcoins.rpc.client.common.{ BitcoindRpcClient, BitcoindVersion }
import org.bitcoins.rpc.config.BitcoindInstance
import org.bitcoins.rpc.serializers.BitcoindJsonSerializers._

/**
 * This class is compatible with version 0.16 of Bitcoin Core.
 */
class BitcoindV16RpcClient(override protected val instance: BitcoindInstance)(
  implicit
  m: ActorMaterializer) extends BitcoindRpcClient(instance) with V16AccountRpc with V16SendRpc {

  override val version: BitcoindVersion = BitcoindVersion.V16

}
