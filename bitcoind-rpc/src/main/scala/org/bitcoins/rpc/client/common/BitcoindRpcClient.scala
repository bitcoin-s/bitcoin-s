package org.bitcoins.rpc.client.common

import java.io.File

import akka.actor.ActorSystem
import org.bitcoins.core.api.chain.db.{
  BlockHeaderDb,
  CompactFilterDb,
  CompactFilterHeaderDb
}
import org.bitcoins.core.api.chain.{ChainApi, ChainQueryApi, FilterSyncMarker}
import org.bitcoins.core.api.feeprovider.FeeRateApi
import org.bitcoins.core.api.node.NodeApi
import org.bitcoins.core.gcs.FilterHeader
import org.bitcoins.core.p2p.CompactFilterMessage
import org.bitcoins.core.protocol.BlockStamp
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import org.bitcoins.rpc.client.v16.BitcoindV16RpcClient
import org.bitcoins.rpc.client.v17.BitcoindV17RpcClient
import org.bitcoins.rpc.client.v18.BitcoindV18RpcClient
import org.bitcoins.rpc.client.v19.BitcoindV19RpcClient
import org.bitcoins.rpc.client.v20.BitcoindV20RpcClient
import org.bitcoins.rpc.config.{BitcoindConfig, BitcoindInstance}

import scala.concurrent.Future

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
    with ChainApi
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
    with PsbtRpc
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
  // Chain Api

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
  override def getFilterCount(): Future[Int] = ???

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
    Future.successful(0)

  // Node Api

  override def broadcastTransaction(transaction: Transaction): Future[Unit] =
    sendRawTransaction(transaction).map(_ => ())

  override def downloadBlocks(
      blockHashes: Vector[DoubleSha256Digest]): Future[Unit] = FutureUtil.unit

  override def processHeaders(headers: Vector[BlockHeader]): Future[ChainApi] =
    Future.successful(this)

  override def processFilterHeaders(
      filterHeaders: Vector[FilterHeader],
      stopHash: DoubleSha256DigestBE): Future[ChainApi] =
    Future.successful(this)

  override def getHeader(
      hash: DoubleSha256DigestBE): Future[Option[BlockHeaderDb]] =
    getBlockHeader(hash).map(header => Some(header.blockHeaderDb))

  override def getHeadersBetween(
      from: BlockHeaderDb,
      to: BlockHeaderDb): Future[Vector[BlockHeaderDb]] = {
    val headerFs =
      from.height.to(to.height).map(height => getHeaderAtHeight(height))
    Future.sequence(headerFs).map(_.toVector)
  }

  private def getHeaderAtHeight(height: Int): Future[BlockHeaderDb] =
    for {
      hash <- getBlockHash(height)
      header <- getBlockHeader(hash)
    } yield header.blockHeaderDb

  override def getHeadersAtHeight(height: Int): Future[Vector[BlockHeaderDb]] =
    getHeaderAtHeight(height).map(header => Vector(header))

  override def getBestBlockHeader(): Future[BlockHeaderDb] =
    for {
      hash <- getBestBlockHash
      header <- getBlockHeader(hash)
    } yield header.blockHeaderDb

  override def nextBlockHeaderBatchRange(
      prevStopHash: DoubleSha256DigestBE,
      batchSize: Int): Future[Option[FilterSyncMarker]] =
    Future.failed(
      new UnsupportedOperationException(
        s"Bitcoind chainApi doesn't allow you fetch block header batch range"))

  override def nextFilterHeaderBatchRange(
      stopHash: DoubleSha256DigestBE,
      batchSize: Int): Future[Option[FilterSyncMarker]] =
    Future.failed(
      new UnsupportedOperationException(
        s"Bitcoind chainApi doesn't allow you fetch filter header batch range"))

  override def processFilters(
      message: Vector[CompactFilterMessage]): Future[ChainApi] =
    Future.successful(this)

  override def processCheckpoints(
      checkpoints: Vector[DoubleSha256DigestBE],
      blockHash: DoubleSha256DigestBE): Future[ChainApi] =
    Future.successful(this)

  override def getFilterHeaderCount(): Future[Int] = ???

  override def getFilterHeadersAtHeight(
      height: Int): Future[Vector[CompactFilterHeaderDb]] =
    filterHeadersUnsupported

  override def getBestFilterHeader(): Future[Option[CompactFilterHeaderDb]] =
    filterHeadersUnsupported

  override def getFilterHeader(
      blockHash: DoubleSha256DigestBE): Future[Option[CompactFilterHeaderDb]] =
    filterHeadersUnsupported

  override def getFilter(
      hash: DoubleSha256DigestBE): Future[Option[CompactFilterDb]] = ???

  override def getFiltersAtHeight(
      height: Int): Future[Vector[CompactFilterDb]] = filterHeadersUnsupported

  protected def filtersUnsupported: Future[Nothing] = {
    Future.failed(
      new UnsupportedOperationException(
        s"bitcoind ${instance.getVersion} does not support block filters"))
  }

  protected def filterHeadersUnsupported: Future[Nothing] = {
    Future.failed(new UnsupportedOperationException(
      s"bitcoind ${instance.getVersion} does not support block filters headers through the rpc"))
  }
}

object BitcoindRpcClient {

  /** The name we give to actor systems we create. We use this
    * information to know which actor systems to shut down
    */
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
      case BitcoindVersion.V20 => BitcoindV20RpcClient.withActorSystem(instance)
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
  val newest: BitcoindVersion = V20

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

  case object V20 extends BitcoindVersion {
    override def toString: String = "v0.20"
  }

  case object Experimental extends BitcoindVersion {
    override def toString: String = "v0.18.99"
  }

  case object Unknown extends BitcoindVersion {
    override def toString: String = "Unknown"
  }

}
