package org.bitcoins.rpc.client

import akka.stream.ActorMaterializer
import org.bitcoins.rpc.config.BitcoindInstance

import scala.concurrent.ExecutionContext

class BitcoindRpcClient(protected val instance: BitcoindInstance)(
  implicit
  m: ActorMaterializer) extends Client
  with BlockchainRpc
  with MessageRpc
  with MempoolRpc
  with MiningRpc
  with MultisigRpc
  with NodeRpc
  with P2PRpc
  with RawTransactionRpc
  with TransacationRpc
  with UTXORpc
  with WalletRpc {

  override val version: BitcoindVersion = UnknownBitcoindVersion

  override protected implicit val executor: ExecutionContext = m.executionContext
  override protected implicit val materializer: ActorMaterializer =
    m

}

sealed trait BitcoindVersion
case object BitcoindV16 extends BitcoindVersion {
  override def toString: String = "v0.16"
}
case object BitcoindV17 extends BitcoindVersion {
  override def toString: String = "v0.17"
}
case object UnknownBitcoindVersion extends BitcoindVersion {
  override def toString: String = "Unknown"
}
