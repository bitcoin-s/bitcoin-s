package org.bitcoins.scripts

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.stream.scaladsl.{Keep, Sink, Source}
import org.bitcoins.core.util.{FutureUtil, NumberUtil}
import org.bitcoins.crypto.{CryptoUtil, DoubleSha256Digest, Sha256Digest}
import org.bitcoins.scripts.GenP2SH.getIter
import org.bitcoins.server.routes.BitcoinSRunner
import org.bitcoins.server.util.BitcoinSAppScalaDaemon
import scodec.bits.ByteVector

import java.time.{Duration, Instant}
import scala.concurrent.Future

class CheckHASH256(implicit
    override val system: ActorSystem
) extends BitcoinSRunner[Unit] {

  private val hash256s = Vector(
    "000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f",
    "3036419579d0abe63b46836a74380f1e2fd4a1e89f3aad616b171ddcdcbede43").map(
    DoubleSha256Digest.apply)
  private val sha256s = Vector(
    "5efe500c58a4847dab87162f88a79f08249b988265d5061696b5d0c94fd8080d",
    "3f6d4081222a35483cdf4cefd128167f133c33e1e0f0b1d638be131a14dc2c5e").map(
    Sha256Digest.apply)
  override def start(): Future[Unit] = {
    val startTime = Instant.now()
    val start = 0L
    val resultF: Future[Seq[(Long, Int, Sha256Digest, DoubleSha256Digest)]] = {
      Source
        .fromIterator(() => getIter(start))
        .mapAsync(FutureUtil.getParallelism) { case (i, byteSize) =>
          Future {
            val max = NumberUtil.pow2(8 * 3)
            val percentage = ((i.toDouble - start) / max.toDouble) * 100
            if (i % 1_000_000 == 0) {
              logger.info(
                s"Generated $i hashes" + f"$percentage%.2f" + s" % time=${Duration
                    .between(startTime, Instant.now())}")
            }
            val bytes = ByteVector.fromLong(i, size = byteSize)
            val sha256 = CryptoUtil.sha256(bytes)
            val hash256 = CryptoUtil.doubleSHA256(bytes)
            (i, byteSize, sha256, hash256)
          }
        }
        .filter { case (_, _, sha256, hash256) =>
          hash256s.contains(hash256) || sha256s.contains(sha256)
        }
        .toMat(Sink.seq)(Keep.right)
        .run()
    }

    resultF.failed.foreach { err =>
      logger.error(s"exn", err)
    }
    resultF.map { result =>
      logger.info(
        s"Done searching, it took=${Duration.between(startTime, Instant.now())}")
      result.foreach { r =>
        logger.info(s"Found hash! $r")
      }
      ()
    }
  }
  override def stop(): Future[Unit] = {
    system.terminate().map(_ => ())
  }
}

object CheckHASH256 extends BitcoinSAppScalaDaemon {
  override val actorSystemName: String =
    s"check-hash256-${System.currentTimeMillis()}"

  override val customFinalDirOpt = None

  new CheckHASH256().run()
}
