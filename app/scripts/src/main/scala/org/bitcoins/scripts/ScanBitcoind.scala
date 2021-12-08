package org.bitcoins.scripts

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.scaladsl.{Keep, Sink, Source}
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction.{Transaction, WitnessTransaction}
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.config.BitcoindRpcAppConfig
import org.bitcoins.server.routes.BitcoinSRunner
import org.bitcoins.server.util.BitcoinSAppScalaDaemon

import java.time.Instant
import scala.concurrent.Future

/** Useful script for scanning bitcoind
  * This file assumes you have pre-configured the connection
  * between bitcoin-s and bitcoind inside of bitcoin-s.conf
  * @see https://bitcoin-s.org/docs/config/configuration#example-configuration-file
  */
class ScanBitcoind()(implicit
    override val system: ActorSystem,
    rpcAppConfig: BitcoindRpcAppConfig)
    extends BitcoinSRunner {

  override def start(): Future[Unit] = {

    val bitcoindF = rpcAppConfig.clientF

    //    val startHeight = 675000
    val endHeightF: Future[Int] = bitcoindF.flatMap(_.getBlockCount)

    val f = for {
      bitcoind <- bitcoindF
      endHeight <- endHeightF
      //_ <- countWitV1MempoolTxs(bitcoind)
      _ <- countTaprootTxsInBlocks(endHeight, 10000, bitcoind)
    } yield ()
    f.failed.foreach(err =>
      logger.error(s"Failed to count witness v1 mempool txs", err))
    Future.unit
  }

  override def stop(): Future[Unit] = {
    system
      .terminate()
      .map(_ => ())
  }

  /** Searches a given Source[Int] that represents block heights applying f to them and returning a Seq[T] with the results */
  def searchBlocks[T](
      bitcoind: BitcoindRpcClient,
      source: Source[Int, NotUsed],
      f: Block => T,
      numParallelism: Int = Runtime.getRuntime.availableProcessors()): Future[
    Seq[T]] = {
    source
      .mapAsync(parallelism = numParallelism) { height =>
        bitcoind
          .getBlockHash(height)
          .flatMap(h => bitcoind.getBlockRaw(h))
          .map(b => (b, height))
      }
      .mapAsync(numParallelism) { case (block, height) =>
        logger.info(
          s"Searching block at height=$height hashBE=${block.blockHeader.hashBE.hex}")
        Future {
          f(block)
        }
      }
      .toMat(Sink.seq)(Keep.right)
      .run()
  }

  def countSegwitTxs(
      bitcoind: BitcoindRpcClient,
      startHeight: Int,
      endHeight: Int): Future[Unit] = {
    val startTime = System.currentTimeMillis()
    val source: Source[Int, NotUsed] = Source(startHeight.to(endHeight))

    //in this simple example, we are going to count the number of witness transactions
    val countSegwitTxs: Block => Int = { block: Block =>
      block.transactions.count(_.isInstanceOf[WitnessTransaction])
    }
    val countsF: Future[Seq[Int]] = for {
      counts <- searchBlocks[Int](bitcoind, source, countSegwitTxs)
    } yield counts

    val countF: Future[Int] = countsF.map(_.sum)

    for {
      count <- countF
      endTime = System.currentTimeMillis()
      _ = logger.info(
        s"Count of segwit txs from height=${startHeight} to endHeight=${endHeight} is ${count}. It took ${endTime - startTime}ms ")
    } yield ()
  }

  def countTaprootTxsInBlocks(
      endHeight: Int,
      lastBlocks: Int,
      bitcoind: BitcoindRpcClient): Future[Int] = {
    val startTime = System.currentTimeMillis()
    val startHeight = endHeight - lastBlocks
    val source: Source[Int, NotUsed] = Source(startHeight.to(endHeight))
    val countTaprootOutputs: Block => Int = { block =>
      val outputs = block.transactions
        .flatMap(_.outputs)
        .filter(_.scriptPubKey.isInstanceOf[WitnessScriptPubKeyV1])
      outputs.length
    }

    val countsF: Future[Seq[Int]] = for {
      counts <- searchBlocks[Int](bitcoind, source, countTaprootOutputs)
    } yield counts

    val countF: Future[Int] = countsF.map(_.sum)

    for {
      count <- countF
      endTime = System.currentTimeMillis()
      _ = logger.info(
        s"Count of taproot outputs from height=${startHeight} to endHeight=${endHeight} is ${count}. It took ${endTime - startTime}ms ")
    } yield count
  }

  def countWitV1MempoolTxs(bitcoind: BitcoindRpcClient): Future[Int] = {
    val memPoolSourceF = getMemPoolSource(bitcoind)
    val countF = memPoolSourceF.flatMap(_.runFold(0) { case (count, tx) =>
      count + tx.outputs.count(
        _.scriptPubKey.isInstanceOf[WitnessScriptPubKeyV1])
    })
    countF.foreach(c =>
      logger.info(
        s"Found $c mempool transactions with witness v1 outputs at ${Instant.now}"))
    countF
  }

  def getMemPoolSource(
      bitcoind: BitcoindRpcClient): Future[Source[Transaction, NotUsed]] = {
    val mempoolF = bitcoind.getRawMemPool
    val sourceF: Future[Source[DoubleSha256DigestBE, NotUsed]] =
      mempoolF.map(Source(_))

    val mempoolTxSourceF: Future[Source[Transaction, NotUsed]] = {
      sourceF.map { source =>
        source.mapAsync(Runtime.getRuntime.availableProcessors()) { hash =>
          bitcoind
            .getRawTransaction(hash)
            .map(_.hex)
        }
      }
    }

    mempoolTxSourceF
  }
}

object ScanBitcoind extends BitcoinSAppScalaDaemon {

  override val actorSystemName: String =
    s"scan-bitcoind-${System.currentTimeMillis()}"

  override val customFinalDirOpt = None

  implicit val rpcAppConfig: BitcoindRpcAppConfig =
    BitcoindRpcAppConfig.fromDefaultDatadir()(system)

  new ScanBitcoind().run()
}
