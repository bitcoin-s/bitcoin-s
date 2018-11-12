package org.bitcoins.rpc.client.common

import akka.stream.ActorMaterializer
import org.bitcoins.rpc.client.v16.BitcoindV16RpcClient
import org.bitcoins.rpc.client.v17.BitcoindV17RpcClient
import org.bitcoins.rpc.config.BitcoindInstance

import scala.concurrent.ExecutionContext

/**
 * This class is not guaranteed to be compatible with any particular
 * version of Bitcoin Core. It implements RPC calls that are similar
 * across different versions. If you need RPC calls specific to a
 * version, check out [[BitcoindV16RpcClient]] or
 * [[BitcoindV17RpcClient]].
 */
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
  with TransactionRpc
  with UTXORpc
  with WalletRpc {

  override val version: BitcoindVersion = BitcoindVersion.Unknown

  override protected implicit val executor: ExecutionContext = m.executionContext
  override protected implicit val materializer: ActorMaterializer =
    m

}

sealed trait BitcoindVersion

object BitcoindVersion {

  case object V16 extends BitcoindVersion {
    override def toString: String = "v0.16"
  }

  case object V17 extends BitcoindVersion {
    override def toString: String = "v0.17"
  }

  case object Unknown extends BitcoindVersion {
    override def toString: String = "Unknown"
  }

}
