package org.bitcoins.scripts

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.stream.{Attributes, IOResult, OverflowStrategy}
import org.apache.pekko.stream.scaladsl.{FileIO, Flow, Keep, Sink, Source}
import org.apache.pekko.util.ByteString
import org.bitcoins.core.config.MainNet
import org.bitcoins.core.protocol.P2SHAddress
import org.bitcoins.core.protocol.script.{
  NonStandardScriptPubKey,
  P2SHScriptPubKey
}
import org.bitcoins.core.util.{FutureUtil, NumberUtil}
import org.bitcoins.scripts.GenP2SH.getIter
import org.bitcoins.server.routes.BitcoinSRunner
import org.bitcoins.server.util.BitcoinSAppScalaDaemon
import play.api.libs.json.Json
import scodec.bits.ByteVector

import java.nio.file.Paths
import java.time.{Duration, Instant}
import scala.concurrent.Future

class GenP2SH()(implicit
    override val system: ActorSystem
) extends BitcoinSRunner[Unit] {
  import Combo._

  private val fileSink: Sink[ByteString, Future[IOResult]] =
    FileIO.toPath(Paths.get("p2sh-size-test.json"))
  private val batchedSink = Flow[String]
    .batch(50_000, { t => Vector(t) }) { (vec, t) => vec.appended(t) }
    .map(batch => ByteString(batch.mkString))
    .toMat(fileSink)(Keep.right)

  override def start(): Future[Unit] = {
    val startTime = Instant.now()
    val start = 0L
    val allCombinations: Iterator[(Long, Int)] = getIter(start)
    val x: Future[IOResult] = Source
      .fromIterator(() => allCombinations)
      .mapAsync(FutureUtil.getParallelism) { case (i, byteSize) =>
        Future {
          val bytes = ByteVector.fromLong(i, size = byteSize)
          val p2sh =
            P2SHScriptPubKey(NonStandardScriptPubKey.fromAsmBytes(bytes))
          val addr = P2SHAddress(p2sh, MainNet)
          val num = bytes.toLong(signed = false)
          val max = NumberUtil.pow2(8 * 3)
          val percentage = ((num.toDouble - start) / max.toDouble) * 100
          if (num % 1_000_000 == 0) {
            logger.info(
              s"Generated $num p2sh addresses " + f"$percentage%.2f" + " %")
          }
          val c = Combo(num = num,
                        p2sh = p2sh.toString,
                        addr = addr.toString,
                        byteSize)
          val json = Json.toJson(c)
          val str = Json.stringify(json) + "\n"
          str
        }
      }
      .buffer(10000,
              OverflowStrategy.backpressure
      ) // Buffer up to 10,000 elements before writing
      .addAttributes(Attributes
        .inputBuffer(initial = 64, max = 256)) // Tune internal buffering
      .runWith(batchedSink)

    x.map { _ =>
      logger.info(s"Done generating p2sh addresses, took=${Duration
          .between(startTime, Instant.now())
          .toMinutes} minutes")
      ()
    }

  }

  override def stop(): Future[Unit] = system.terminate().map(_ => ())
}

object GenP2SH extends BitcoinSAppScalaDaemon {

  def getIter(start: Long): Iterator[(Long, Int)] = {
    val byteSizes = 0.until(4)

    val allCombinations: Iterator[(Long, Int)] = byteSizes
      .map { byteSize =>
        Iterator
          .iterate(start)(_ + 1)
          .takeWhile(_ < NumberUtil.pow2(8 * byteSize))
          .map(l => (l, byteSize))
          .iterator
      }
      .iterator
      .flatten
    allCombinations
  }

  override val actorSystemName: String =
    s"gen-p2sh-${System.currentTimeMillis()}"

  override val customFinalDirOpt = None

//  implicit val rpcAppConfig: BitcoindRpcAppConfig =
//    BitcoindRpcAppConfig.fromDefaultDatadir()(system)

  new GenP2SH().run()
}
