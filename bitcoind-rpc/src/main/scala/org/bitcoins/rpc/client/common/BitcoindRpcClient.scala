package org.bitcoins.rpc.client.common

import java.io.File

import org.bitcoins.rpc.config.{BitcoindConfig, BitcoindInstance}
import scala.concurrent.ExecutionContext

/**
  * This class is not guaranteed to be compatible with any particular
  * version of Bitcoin Core. It implements RPC calls that are similar
  * across different versions. If you need RPC calls specific to a
  * version, check out
  * [[org.bitcoins.rpc.client.v16.BitcoindV16RpcClient BitcoindV16RpcClient]]
  * or
  * [[org.bitcoins.rpc.client.v17.BitcoindV17RpcClient BitcoindV17RpcClient]].
  *
  * If a RPC call fails for any reason, a
  * [[org.bitcoins.rpc.BitcoindException BitcoindException]] is thrown.
  * This is a sealed abstract class, so you can pattern match easily
  * on the errors, and handle them as you see fit.
  */
class BitcoindRpcClient(val instance: BitcoindInstance)(
    implicit ec: ExecutionContext)
    extends Client
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
    with WalletRpc
    with UtilRpc {

  override def version: BitcoindVersion = BitcoindVersion.Unknown
  require(version == BitcoindVersion.Unknown || version == instance.getVersion,
          s"bitcoind version must be $version, got ${instance.getVersion}")

}

object BitcoindRpcClient {

  /**
    * Constructs a RPC client from the given datadir, or
    * the default datadir if no directory is provided
    */
  def fromDatadir(datadir: File = BitcoindConfig.DEFAULT_DATADIR)(
      implicit ec: ExecutionContext): BitcoindRpcClient = {
    val instance = BitcoindInstance.fromDatadir(datadir)
    val cli = new BitcoindRpcClient(instance)
    cli
  }
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
