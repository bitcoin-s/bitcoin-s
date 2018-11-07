package org.bitcoins.rpc.client

import akka.stream.ActorMaterializer
import org.bitcoins.rpc.config.BitcoindInstance

import scala.concurrent.ExecutionContext

class BitcoindRpcClient(protected val instance: BitcoindInstance)(
  implicit
  m: ActorMaterializer) extends Client
  with BlockchainRpc
  with P2PRpc
  with WalletRpc
  with MempoolRpc
  with MiningRpc
  with RawTransactionRpc
  with TransacationRpc
  with UTXORpc
  with NodeRpc {
  override protected implicit val executor: ExecutionContext = m.executionContext
  override protected implicit val materializer: ActorMaterializer =
    m
}
