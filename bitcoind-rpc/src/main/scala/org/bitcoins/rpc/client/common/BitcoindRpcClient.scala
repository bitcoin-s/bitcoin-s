package org.bitcoins.rpc.client.common

import java.io.File

import akka.actor.ActorSystem
import org.bitcoins.rpc.config.{BitcoindConfig, BitcoindInstance}

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
    implicit
    override val system: ActorSystem)
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

  /** The name we give to actor systems we create. We use this
    * information to know which actor systems to shut down */
  private[rpc] val ActorSystemName = "bitcoind-rpc-client-created-by-bitcoin-s"

  /**
    * Creates an RPC client from the given instance.
    *
    * Behind the scenes, we create an actor system for
    * you. You can use `withActorSystem` if you want to
    * manually specify an actor system for the RPC client.
    */
  def apply(instance: BitcoindInstance): BitcoindRpcClient = {
    implicit val system = ActorSystem.create(ActorSystemName)
    withActorSystem(instance)
  }

  /**
    * Creates an RPC client from the given instance,
    * together with the given actor system. This is for
    * advanced users, wher you need fine grained control
    * over the RPC client.
    */
  def withActorSystem(instance: BitcoindInstance)(
      implicit system: ActorSystem): BitcoindRpcClient =
    new BitcoindRpcClient(instance)

  /**
    * Constructs a RPC client from the given datadir, or
    * the default datadir if no directory is provided
    */
  def fromDatadir(
      datadir: File = BitcoindConfig.DEFAULT_DATADIR,
      binary: File): BitcoindRpcClient = {
    val instance = BitcoindInstance.fromDatadir(datadir, binary)
    val cli = BitcoindRpcClient(instance)
    cli
  }
}

sealed trait BitcoindVersion

object BitcoindVersion {

  /** The newest version of `bitcoind` we support */
  val newest = V19

  case object V16 extends BitcoindVersion {
    override def toString: String = "v0.16"
  }

  case object V17 extends BitcoindVersion {
    override def toString: String = "v0.17"
  }

  case object V18 extends BitcoindVersion {
    override def toString: String = "v0.18"
  }

  case object V19 extends BitcoindVersion {
    override def toString: String = "v0.19"
  }

  case object Experimental extends BitcoindVersion {
    override def toString: String = "v0.18.99"
  }

  case object Unknown extends BitcoindVersion {
    override def toString: String = "Unknown"
  }

}
