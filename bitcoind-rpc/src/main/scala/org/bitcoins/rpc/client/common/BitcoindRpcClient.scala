package org.bitcoins.rpc.client.common

import org.apache.pekko.Done
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.stream.scaladsl.{Keep, RunnableGraph, Sink, Source}
import org.bitcoins.commons.jsonmodels.bitcoind.{
  GetNetworkInfoResultPostV21,
  GetNetworkInfoResultV28
}
import org.bitcoins.commons.util.BitcoinSLogger
import org.bitcoins.core.api.chain.db.BlockHeaderDb
import org.bitcoins.core.api.chain.{ChainApi, FilterSyncMarker}
import org.bitcoins.core.api.feeprovider.FeeRateApi
import org.bitcoins.core.api.node.NodeApi
import org.bitcoins.core.gcs.FilterHeader
import org.bitcoins.core.p2p.CompactFilterMessage
import org.bitcoins.core.protocol.{BitcoinAddress, BlockStamp}
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.util.{FutureUtil, NetworkUtil}
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.crypto.{DoubleSha256DigestBE, StringFactory}
import org.bitcoins.rpc.client.v18.V18AssortedRpc
import org.bitcoins.rpc.client.v20.V20MultisigRpc
import org.bitcoins.rpc.client.v27.BitcoindV27RpcClient
import org.bitcoins.rpc.client.v28.BitcoindV28RpcClient
import org.bitcoins.rpc.client.v29.BitcoindV29RpcClient
import org.bitcoins.rpc.client.v30.BitcoindV30RpcClient
import org.bitcoins.rpc.config.*

import java.io.File
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.Future

/** This class is not guaranteed to be compatible with any particular version of
  * Bitcoin Core. It implements RPC calls that are similar across different
  * versions. If you need RPC calls specific to a version, check out
  *
  * If a RPC call fails for any reason, a
  * [[BitcoindException BitcoindException]] is thrown. This is a sealed abstract
  * class, so you can pattern match easily on the errors, and handle them as you
  * see fit.
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

  def bitcoindRpcAppConfig: BitcoindRpcAppConfig = instance.bitcoindRpcAppConfig

  override lazy val version: Future[BitcoindVersion] = {
    import org.bitcoins.commons.serializers.JsonSerializers.{
      getNetworkInfoV28Reads,
      getNetworkInfoPostV21Reads
    }
    instance match {
      case _: BitcoindInstanceRemote =>
        // work around for version specific calls to 'getnetworkinfo'
        // the return payload is slightly different pre28 and post 28
        // this can be removed in the future when we drop support for v27 of bitcoind
        bitcoindCall[GetNetworkInfoResultV28]("getnetworkinfo")
          .recoverWith { _ =>
            bitcoindCall[GetNetworkInfoResultPostV21]("getnetworkinfo")
          }
          .map(result => BitcoindVersion.fromNetworkVersion(result.version))
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
  ): Future[Unit] = {
    val callback =
      bitcoindRpcAppConfig.callBacks.executeOnBlockReceivedCallbacks(_)
    val graph: RunnableGraph[Future[Done]] = Source(blockHashes)
      .mapAsync(FutureUtil.getParallelism)(getBlockRaw)
      .toMat(Sink.foreachAsync(1)(callback))(Keep.right)

    graph
      .run()
      .map(_ => ())
  }

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
    val range = from.height.to(to.height).toVector
    val headerFs =
      Future.traverse(range)(height => getHeaderAtHeight(height))
    headerFs
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
    //see: https://github.com/bitcoin/bitcoin/issues/33618#issuecomment-3402590889
    val address = BitcoinAddress("2NFyxovf6MyxfHqtVjstGzs6HeLqv92Nq4U")
    for {
      blocks <- generateToAddress(numBlocks, address)
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
  val DEFAULT_WALLET_NAME: String = "wallet.dat"

  /** Creates an RPC client from the given instance, together with the given
    * actor system. This is for advanced users, where you need fine grained
    * control over the RPC client.
    */
  def apply(instance: BitcoindInstanceLocal)(implicit
      system: ActorSystem
  ): BitcoindRpcClient =
    new BitcoindRpcClient(instance)(system)

  /** Constructs a RPC client from the given datadir, or the default datadir if
    * no directory is provided. This is always a [[BitcoindInstanceLocal]] since
    * a binary is passed into this method
    */
  def fromDatadir(
      datadir: File = BitcoindConfig.DEFAULT_DATADIR,
      binary: File
  )(implicit system: ActorSystem): BitcoindRpcClient = {
    val instance = BitcoindInstanceLocal.fromDatadir(datadir, binary)
    val cli = BitcoindRpcClient(instance)
    cli
  }

  /** Returns a bitcoind with the appropriated version you passed in, the
    * bitcoind is NOT started.
    */
  def fromVersion(version: BitcoindVersion, instance: BitcoindInstanceLocal)(
      implicit system: ActorSystem
  ): BitcoindRpcClient = {
    val bitcoind = version match {
      case BitcoindVersion.V27 => BitcoindV27RpcClient(instance)
      case BitcoindVersion.V28 => BitcoindV28RpcClient(instance)
      case BitcoindVersion.V29 => BitcoindV29RpcClient(instance)
      case BitcoindVersion.V30 => BitcoindV30RpcClient(instance)
      case BitcoindVersion.Unknown =>
        sys.error(
          s"Cannot create a Bitcoin Core RPC client: unsupported version"
        )
    }
    bitcoind
  }

  /** Returns a bitcoind with the appropriated version you passed in, the
    * bitcoind is NOT started.
    */
  def fromVersion(version: BitcoindVersion, instance: BitcoindInstanceRemote)(
      implicit system: ActorSystem
  ): BitcoindRpcClient = {
    val bitcoind = version match {
      case BitcoindVersion.V27 => new BitcoindV27RpcClient(instance)
      case BitcoindVersion.V28 => new BitcoindV28RpcClient(instance)
      case BitcoindVersion.V29 => new BitcoindV29RpcClient(instance)
      case BitcoindVersion.V30 => new BitcoindV30RpcClient(instance)
      case BitcoindVersion.Unknown =>
        sys.error(
          s"Cannot create a Bitcoin Core RPC client: unsupported version"
        )
    }
    bitcoind
  }
}

sealed trait BitcoindVersion

object BitcoindVersion
    extends StringFactory[BitcoindVersion]
    with BitcoinSLogger {

  /** The newest version of `bitcoind` we support */
  val newest: BitcoindVersion = V30

  val standard: Vector[BitcoindVersion] =
    Vector(V30, V29, V28, V27)

  val known: Vector[BitcoindVersion] = standard

  case object V27 extends BitcoindVersion {
    override def toString: String = "v27.2"
  }

  case object V28 extends BitcoindVersion {
    override def toString: String = "v28.2"
  }

  case object V29 extends BitcoindVersion {
    override def toString: String = "v29.0"
  }

  case object V30 extends BitcoindVersion {
    override def toString: String = "v30.0"
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
      case "27" => V27
      case "28" => V28
      case "29" => V29
      case _ =>
        logger.warn(
          s"Unsupported Bitcoin Core version: $int. The latest supported version is ${BitcoindVersion.newest}"
        )
        newest
    }
  }

  def findVersion(version: String): Option[BitcoindVersion] = {
    // first try to match the version exactly
    BitcoindVersion.known
      .find(v => version == v.toString) match {
      case Some(r) => Some(r)
      case None    =>
        // try to match the major version if we can't match it exactly
        BitcoindVersion.known.find { v =>
          version.startsWith(v.toString)
        }
    }
  }
}
