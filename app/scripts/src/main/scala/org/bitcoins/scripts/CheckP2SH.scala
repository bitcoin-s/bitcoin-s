package org.bitcoins.scripts

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.stream.{IOResult, OverflowStrategy}
import org.apache.pekko.stream.scaladsl.{FileIO, Framing, Keep, Sink, Source}
import org.apache.pekko.util.ByteString
import org.bitcoins.commons.jsonmodels.bitcoind.{
  RpcOpts,
  ScanTxoutSetRequest,
  ScanTxoutSetResult
}
import org.bitcoins.core.protocol.script.descriptor.{
  AddressDescriptor,
  Descriptor
}
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.rpc.config.BitcoindRpcAppConfig
import org.bitcoins.server.routes.BitcoinSRunner
import org.bitcoins.server.util.BitcoinSAppScalaDaemon
import play.api.libs.json.Json

import java.nio.file.{Files, Path, Paths}
import java.time.{Duration, Instant}
import scala.concurrent.{ExecutionContext, Future}

case class CheckP2SH()(implicit
    override val system: ActorSystem,
    rpcAppConfig: BitcoindRpcAppConfig
) extends BitcoinSRunner[Unit] {
  private val bitcoindF = rpcAppConfig.clientF

  val filePath: Path = Paths.get("p2sh-7.json")
  val lineCount = Files.lines(filePath).count() // Uses Java Streams API
  override def start(): Future[Unit] = {
    val start = Instant.now()
    val grouping = 645_000
    val drop = 230_265_000
    var count = drop
    val fmt = { combo: Combo => AddressDescriptor(combo.bitcoinAddress) }

    val parallelism = 1
    val source =
      CheckP2SH.getP2SHSource(filePath, fmt, grouping, drop, parallelism)
    val resultF: Future[Seq[ScanTxoutSetResult]] = source
      .mapAsync(parallelism) { descs: Vector[Descriptor] =>
        val batchStart = Instant.now()
        val req = ScanTxoutSetRequest(action = RpcOpts.ScanBlocksOpt.Start,
                                      descs = descs)
        count += descs.length
        val percent = (count.toDouble / lineCount) * 100.0
        val totalTime = Duration.between(start, Instant.now())
        logger.info(
          s"Searching $count addresses " + f"$percent%.2f" + s"% total time=$totalTime")
        bitcoindF.flatMap(_.scanTxoutSet(req)).map { result =>
          val secs = Duration.between(batchStart, Instant.now).getSeconds
          logger.info(s"result=$result it took ${secs}secs")
          result
        }
      }
      .filter(_.txouts > 0)
      .toMat(Sink.seq)(Keep.right)
      .run()

    resultF.map { results =>
      logger.info(
        s"Done scanning p2sh, found ${results.length} relevant blocks, it took ${Duration
            .between(start, Instant.now())}minutes")

      results.foreach(r => logger.info(s"r=$r"))
      ()
    }
  }

  override def stop(): Future[Unit] = {
    system.terminate().map(_ => ())
  }
}

object CheckP2SH extends BitcoinSAppScalaDaemon {

  override val actorSystemName: String =
    s"check-p2sh-${System.currentTimeMillis()}"

  override val customFinalDirOpt = None

  implicit val rpcAppConfig: BitcoindRpcAppConfig =
    BitcoindRpcAppConfig.fromDefaultDatadir()(system)

  def getP2SHSource[T](
      filePath: Path,
      fmt: Combo => T,
      grouping: Int,
      drop: Int,
      parallelism: Int = FutureUtil.getParallelism)(implicit
      ec: ExecutionContext): Source[Vector[T], Future[IOResult]] = {
    val initDrop = drop - grouping
    val source: Source[Vector[T], Future[IOResult]] = FileIO
      .fromPath(filePath, chunkSize = 8_388_608) // 8mb
      .buffer(128, OverflowStrategy.backpressure)
      .async
      .via(
        Framing.delimiter(delimiter = ByteString("\n"),
                          maximumFrameLength = 1024,
                          allowTruncation = false)
      )
      .drop(initDrop)
      .mapAsync(FutureUtil.getParallelism) { line =>
        Future {
          val json = Json.parse(line.utf8String)
          val c = json.validate[Combo](Combo.reads).get
          fmt(c)
        }
      } // Read line by line
      .buffer(grouping * parallelism, OverflowStrategy.backpressure)
      .batch(grouping, { t => Vector(t) }) { case (accum, t) =>
        accum.appended(t)
      }

    source
  }

  new CheckP2SH().run()
}
