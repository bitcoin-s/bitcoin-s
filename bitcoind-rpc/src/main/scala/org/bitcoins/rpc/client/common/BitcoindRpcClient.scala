package org.bitcoins.rpc.client.common

import org.apache.pekko.actor.ActorSystem
import org.bitcoins.commons.util.BitcoinSLogger
import org.bitcoins.core.api.chain.db.BlockHeaderDb
import org.bitcoins.core.api.chain.{ChainApi, FilterSyncMarker}
import org.bitcoins.core.api.feeprovider.FeeRateApi
import org.bitcoins.core.api.node.NodeApi
import org.bitcoins.core.gcs.FilterHeader
import org.bitcoins.core.p2p.CompactFilterMessage
import org.bitcoins.core.protocol.BlockStamp
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.util.{FutureUtil, NetworkUtil}
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.crypto.{DoubleSha256DigestBE, StringFactory}
import org.bitcoins.rpc.client.v18.V18AssortedRpc
import org.bitcoins.rpc.client.v20.V20MultisigRpc
import org.bitcoins.rpc.client.v25.BitcoindV25RpcClient
import org.bitcoins.rpc.client.v26.BitcoindV26RpcClient
import org.bitcoins.rpc.config._

import java.io.File
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.Future

/** This class is not guaranteed to be compatible with any particular version of
  * Bitcoin Core. It implements RPC calls that are similar across different
  * versions. If you need RPC calls specific to a version, check out
  *
  * If a RPC call fails for any reason, a
  * [[org.bitcoins.rpc.BitcoindException BitcoindException]] is thrown. This is
  * a sealed abstract class, so you can pattern match easily on the errors, and
  * handle them as you see fit.
  */
class BitcoindRpcClient(override val instance: BitcoindInstance)(implicit
    override val system: ActorSystem
) extends Client
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
    with UtilRpc
    with V18AssortedRpc
    with DescriptorRpc
    with V20MultisigRpc {

  private val syncing = new AtomicBoolean(false)

  override def version: Future[BitcoindVersion] = {
    instance match {
      case _: BitcoindInstanceRemote =>
        getNetworkInfo.map(info =>
          BitcoindVersion.fromNetworkVersion(info.version))
      case local: BitcoindInstanceLocal =>
        Future.successful(local.getVersion)
    }
  }

  // Fee Rate Provider

  override def getFeeRate(): Future[FeeUnit] =
    estimateSmartFee(blocks = 6).flatMap { result =>
      result.feerate match {
        case Some(feeRate) => Future.successful(feeRate)
        case None =>
          Future.failed(
            new RuntimeException(
              s"Unexpected error when getting fee rate, errors=${result.errors}"
            )
          )
      }
    }
  // Chain Api

  /** Gets the height of the given block */
  override def getBlockHeight(
      blockHash: DoubleSha256DigestBE
  ): Future[Option[Int]] = {
    getBlockHeader(blockHash).map(header => Some(header.height))
  }

  /** Gets number of confirmations for the given block hash */
  override def getNumberOfConfirmations(
      blockHash: DoubleSha256DigestBE
  ): Future[Option[Int]] = {
    getBlockHeader(blockHash).map(header => Some(header.confirmations))
  }

  /** Returns the block height of the given block stamp */
  override def getHeightByBlockStamp(blockStamp: BlockStamp): Future[Int] =
    blockStamp match {
      case blockHeight: BlockStamp.BlockHeight =>
        Future.successful(blockHeight.height)
      case blockHash: BlockStamp.BlockHash =>
        getBlockHeader(blockHash.hash).map(_.height)
      case blockTime: BlockStamp.BlockTime =>
        Future.failed(
          new UnsupportedOperationException(s"Not implemented: $blockTime")
        )
    }

  /** Gets the block height of the closest block to the given time */
  override def epochSecondToBlockHeight(time: Long): Future[Int] = {
    require(
      time >= 1231006505L,
      s"Time must be after the genesis block (1231006505), got $time"
    )

    // the longest difference between successive blocks ever recorded + 10 minutes
    val MaxDiff = 463160L + 600L

    def binarySearch(l: Int, r: Int): Future[Int] = {
      if (l > r) {
        Future.successful(0)
      } else {
        val m = l + (r - l) / 2
        (for {
          blockHash <- getBlockHash(m)
          blockHeader <- getBlockHeader(blockHash)
        } yield {
          val diff = time - blockHeader.time.toLong
          if (diff >= 0L && diff <= MaxDiff) {
            Future.successful(blockHeader.height)
          } else if (diff < 0L) {
            binarySearch(l, m - 1)
          } else {
            binarySearch(m + 1, r)
          }
        }).flatten
      }
    }

    for {
      bestBlock <- getBestBlockHeader()
      blockHeight <- binarySearch(0, bestBlock.height)
    } yield {
      blockHeight
    }
  }

  override def getMedianTimePast(): Future[Long] = {
    for {
      info <- getBlockChainInfo
    } yield info.mediantime.toLong
  }

  // Node Api

  override def broadcastTransactions(
      transactions: Vector[Transaction]
  ): Future[Unit] =
    FutureUtil.sequentially(transactions)(sendRawTransaction(_)).map(_ => ())

  override def downloadBlocks(
      blockHashes: Vector[DoubleSha256DigestBE]
  ): Future[Unit] = Future.unit

  override def processHeaders(headers: Vector[BlockHeader]): Future[ChainApi] =
    Future.successful(this)

  override def processFilterHeaders(
      filterHeaders: Vector[FilterHeader],
      stopHash: DoubleSha256DigestBE
  ): Future[ChainApi] =
    Future.successful(this)

  override def getHeader(
      hash: DoubleSha256DigestBE
  ): Future[Option[BlockHeaderDb]] =
    getBlockHeader(hash).map(header => Some(header.blockHeaderDb))

  override def getHeaders(
      hashes: Vector[DoubleSha256DigestBE]
  ): Future[Vector[Option[BlockHeaderDb]]] = {
    // sends a request for every header, i'm not aware of a way to batch these
    val resultsNested: Vector[Future[Option[BlockHeaderDb]]] =
      hashes.map(getHeader)
    Future
      .sequence(resultsNested)
  }

  override def getHeadersBetween(
      from: BlockHeaderDb,
      to: BlockHeaderDb
  ): Future[Vector[BlockHeaderDb]] = {
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

  override def getBestChainTips(): Future[Vector[BlockHeaderDb]] = {
    getChainTips.flatMap(tips =>
      Future.traverse(tips) { tip =>
        getBlockHeader(tip.hash).map(_.blockHeaderDb)
      })
  }

  override def getBestBlockHeader(): Future[BlockHeaderDb] =
    for {
      hash <- getBestBlockHash()
      header <- getBlockHeader(hash)
    } yield header.blockHeaderDb

  override def nextBlockHeaderBatchRange(
      prevStopHash: DoubleSha256DigestBE,
      stopHash: DoubleSha256DigestBE,
      batchSize: Int
  ): Future[Option[FilterSyncMarker]] =
    Future.failed(
      new UnsupportedOperationException(
        s"Bitcoind chainApi doesn't allow you fetch block header batch range"
      )
    )

  override def nextFilterHeaderBatchRange(
      stopBlockHash: DoubleSha256DigestBE,
      batchSize: Int,
      startHeightOpt: Option[Int]
  ): Future[Option[FilterSyncMarker]] =
    Future.failed(
      new UnsupportedOperationException(
        s"Bitcoind chainApi doesn't allow you fetch filter header batch range"
      )
    )

  override def processFilters(
      message: Vector[CompactFilterMessage]
  ): Future[ChainApi] =
    Future.successful(this)

  override def processCheckpoints(
      checkpoints: Vector[DoubleSha256DigestBE],
      blockHash: DoubleSha256DigestBE
  ): Future[ChainApi] =
    Future.successful(this)

  def generate(numBlocks: Int): Future[Vector[DoubleSha256DigestBE]] = {
    for {
      addr <- getNewAddress
      blocks <- generateToAddress(numBlocks, addr)
    } yield blocks
  }

  override def isSyncing(): Future[Boolean] = Future.successful(syncing.get())

  override def isIBD(): Future[Boolean] = {
    getBlockChainInfo.map(_.initialblockdownload)
  }

  override def isTipStale(): Future[Boolean] = {
    getBestBlockHeader().map { blockHeaderDb =>
      NetworkUtil.isBlockHeaderStale(
        blockHeaderDb.blockHeader,
        network.chainParams
      )
    }
  }

  override def setSyncing(value: Boolean): Future[ChainApi] = {
    syncing.set(value)
    Future.successful(this)
  }

  override def setIBD(value: Boolean): Future[ChainApi] = {
    logger.warn(s"Cannot set IBD of BitcoindRpcClient, this is a noop")
    Future.successful(this)
  }

  override def getConnectionCount: Future[Int] = {
    super[P2PRpc].getConnectionCount
  }
}

object BitcoindRpcClient {

  /** The name we give to actor systems we create. We use this information to
    * know which actor systems to shut down
    */
  private[rpc] val ActorSystemName = "bitcoind-rpc-client-created-by-bitcoin-s"

  implicit private lazy val system: ActorSystem =
    ActorSystem.create(ActorSystemName)

  val DEFAULT_WALLET_NAME: String = "wallet.dat"

  /** Creates an RPC client from the given instance.
    *
    * Behind the scenes, we create an actor system for you. You can use
    * `withActorSystem` if you want to manually specify an actor system for the
    * RPC client.
    */
  def apply(instance: BitcoindInstance): BitcoindRpcClient = {
    withActorSystem(instance)(system)
  }

  /** Creates an RPC client from the given instance, together with the given
    * actor system. This is for advanced users, where you need fine grained
    * control over the RPC client.
    */
  def withActorSystem(instance: BitcoindInstance)(implicit
      system: ActorSystem
  ): BitcoindRpcClient =
    new BitcoindRpcClient(instance)

  /** Constructs a RPC client from the given datadir, or the default datadir if
    * no directory is provided. This is always a [[BitcoindInstanceLocal]] since
    * a binary is passed into this method
    */
  def fromDatadir(
      datadir: File = BitcoindConfig.DEFAULT_DATADIR,
      binary: File
  ): BitcoindRpcClient = {
    val instance = BitcoindInstanceLocal.fromDatadir(datadir, binary)
    val cli = BitcoindRpcClient(instance)
    cli
  }

  /** Returns a bitcoind with the appropriated version you passed in, the
    * bitcoind is NOT started.
    */
  def fromVersion(version: BitcoindVersion, instance: BitcoindInstance)(implicit
      system: ActorSystem
  ): BitcoindRpcClient = {
    val bitcoind = version match {
      case BitcoindVersion.V25 => BitcoindV25RpcClient.withActorSystem(instance)
      case BitcoindVersion.V26 => BitcoindV26RpcClient.withActorSystem(instance)
      case BitcoindVersion.Unknown =>
        sys.error(
          s"Cannot create a Bitcoin Core RPC client: unsupported version"
        )
    }
    bitcoind
  }

  def fromVersionNoSystem(
      version: BitcoindVersion,
      instance: BitcoindInstance
  ): BitcoindRpcClient = {
    fromVersion(version, instance)(system)
  }
}

sealed trait BitcoindVersion

object BitcoindVersion
    extends StringFactory[BitcoindVersion]
    with BitcoinSLogger {

  /** The newest version of `bitcoind` we support */
  val newest: BitcoindVersion = V26

  val standard: Vector[BitcoindVersion] =
    Vector(V25)

  val known: Vector[BitcoindVersion] = standard

  case object V25 extends BitcoindVersion {
    override def toString: String = "v25"
  }

  case object V26 extends BitcoindVersion {
    override def toString: String = "v26"
  }

  case object Unknown extends BitcoindVersion {
    override def toString: String = "Unknown"
  }

  override def fromStringOpt(str: String): Option[BitcoindVersion] = {
    known.find(_.toString.toLowerCase == str.toLowerCase)
  }

  override def fromString(string: String): BitcoindVersion = {
    fromStringOpt(string).get
  }

  /** Gets the bitcoind version from the 'getnetworkresult' bitcoind rpc An
    * example for 210100 for the 21.1.0 release of bitcoin core
    */
  def fromNetworkVersion(int: Int): BitcoindVersion = {
    // need to translate the int 210100 (as an example) to a BitcoindVersion
    int.toString.substring(0, 2) match {
      case "25" => V25
      case _ =>
        logger.warn(
          s"Unsupported Bitcoin Core version: $int. The latest supported version is ${BitcoindVersion.newest}"
        )
        newest
    }
  }
}
