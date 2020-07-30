package org.bitcoins.rpc.client.common

import java.io.File
import java.nio.file.{Files, Path}

import akka.actor.ActorSystem
import org.bitcoins.core.api.{ChainQueryApi, FeeRateApi, NodeApi}
import org.bitcoins.core.compat.JavaConverters._
import org.bitcoins.core.protocol.BlockStamp
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import org.bitcoins.rpc.client.v16.BitcoindV16RpcClient
import org.bitcoins.rpc.client.v17.BitcoindV17RpcClient
import org.bitcoins.rpc.client.v18.BitcoindV18RpcClient
import org.bitcoins.rpc.client.v19.BitcoindV19RpcClient
import org.bitcoins.rpc.config.{BitcoindConfig, BitcoindInstance}

import scala.annotation.tailrec
import scala.concurrent.Future
import scala.util.Properties

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
class BitcoindRpcClient(val instance: BitcoindInstance)(implicit
    override val system: ActorSystem)
    extends Client
    with FeeRateApi
    with NodeApi
    with ChainQueryApi
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

  // Fee Rate Provider

  override def getFeeRate: Future[FeeUnit] =
    estimateSmartFee(blocks = 6).flatMap { result =>
      result.feerate match {
        case Some(feeRate) => Future.successful(feeRate)
        case None =>
          Future.failed(
            new RuntimeException("Unexpected error when getting fee rate"))
      }
    }
  // Chain Query Api

  /** Gets the height of the given block */
  override def getBlockHeight(
      blockHash: DoubleSha256DigestBE): Future[Option[Int]] = {
    getBlockHeader(blockHash).map(header => Some(header.height))
  }

  /** Gets number of confirmations for the given block hash */
  override def getNumberOfConfirmations(
      blockHash: DoubleSha256DigestBE): Future[Option[Int]] = {
    getBlockHeader(blockHash).map(header => Some(header.confirmations))
  }

  /** Gets the number of compact filters in the database */
  override def getFilterCount: Future[Int] = ???

  /** Returns the block height of the given block stamp */
  override def getHeightByBlockStamp(blockStamp: BlockStamp): Future[Int] =
    blockStamp match {
      case blockHeight: BlockStamp.BlockHeight =>
        Future.successful(blockHeight.height)
      case blockHash: BlockStamp.BlockHash =>
        getBlockHeader(blockHash.hash).map(_.height)
      case blockTime: BlockStamp.BlockTime =>
        Future.failed(
          new UnsupportedOperationException(s"Not implemented: $blockTime"))
    }

  override def getFiltersBetweenHeights(
      startHeight: Int,
      endHeight: Int): Future[Vector[ChainQueryApi.FilterResponse]] = ???

  /** Gets the block height of the closest block to the given time */
  override def epochSecondToBlockHeight(time: Long): Future[Int] =
    Future.failed(
      new UnsupportedOperationException(
        "epochSecondToBlockHeight is not supported by bitcoind"))

  // Node Api

  override def broadcastTransaction(transaction: Transaction): Future[Unit] =
    sendRawTransaction(transaction).map(_ => ())

  override def downloadBlocks(
      blockHashes: Vector[DoubleSha256Digest]): Future[Unit] = FutureUtil.unit
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
  def withActorSystem(instance: BitcoindInstance)(implicit
      system: ActorSystem): BitcoindRpcClient =
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

  /** Returns a bitcoind with the appropriated version you passed in, the bitcoind is NOT started. */
  def fromVersion(version: BitcoindVersion, instance: BitcoindInstance)(implicit
      system: ActorSystem): BitcoindRpcClient = {
    val bitcoind = version match {
      case BitcoindVersion.V16 => BitcoindV16RpcClient.withActorSystem(instance)
      case BitcoindVersion.V17 => BitcoindV17RpcClient.withActorSystem(instance)
      case BitcoindVersion.V18 => BitcoindV18RpcClient.withActorSystem(instance)
      case BitcoindVersion.V19 => BitcoindV19RpcClient.withActorSystem(instance)
      case BitcoindVersion.Experimental =>
        BitcoindV18RpcClient.withActorSystem(instance)
      case BitcoindVersion.Unknown =>
        sys.error(
          s"Cannot create a bitcoind from a unknown or experimental version")
    }

    bitcoind
  }
}

sealed trait BitcoindVersion

object BitcoindVersion {

  /** The newest version of `bitcoind` we support */
  val newest: BitcoindVersion = V19

  val known = Vector(V16, V17, V18, V19, Experimental)

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

  def fromString(str: String): Option[BitcoindVersion] = {
    known.find(_.toString.toLowerCase == str.toLowerCase)
  }

  @tailrec
  private[bitcoins] def getBitcoindBinary(
      binaryDirectory: Path,
      version: BitcoindVersion): File =
    version match {
      // default to newest version
      case Unknown => getBitcoindBinary(binaryDirectory, BitcoindVersion.newest)
      case known @ (Experimental | V16 | V17 | V18 | V19) =>
        val dir = binaryDirectory.resolve("bitcoind")
        val fileList = Files
          .list(dir)
          .iterator()
          .asScala
          .toList
          .filter(f => Files.isDirectory(f))
        // drop leading 'v'
        val version = known.toString.drop(1)
        val filtered =
          if (known == Experimental)
            // we want exact match for the experimental version
            fileList
              .filter(f => f.toString.endsWith(version))
          else
            // we don't want the experimental version to appear in the list along with the production ones
            fileList
              .filterNot(f =>
                f.toString.endsWith(Experimental.toString.drop(1)))
              .filter(f => f.toString.contains(version))

        if (filtered.isEmpty)
          throw new RuntimeException(
            s"bitcoind ${known.toString} is not installed in $dir. Run `sbt downloadBitcoind`")

        // might be multiple versions downloaded for
        // each major version, i.e. 0.16.2 and 0.16.3
        val versionFolder = filtered.max

        versionFolder
          .resolve("bin")
          .resolve(if (Properties.isWin) "bitcoind.exe" else "bitcoind")
          .toFile
    }
}
