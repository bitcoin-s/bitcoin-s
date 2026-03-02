package org.bitcoins.scripts
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.stream.scaladsl.{FileIO, Framing, Sink}
import org.apache.pekko.util.ByteString
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.rpc.config.BitcoindRpcAppConfig
import org.bitcoins.scripts.ReadNegativeNumberResult.NegativeNumberResult
import org.bitcoins.server.routes.BitcoinSRunner
import org.bitcoins.server.util.BitcoinSAppScalaDaemon
import play.api.libs.json.{Json, Reads, Writes}

import java.nio.file.Paths
import scala.concurrent.Future

case class ReadNegativeNumberResult()(implicit override val system: ActorSystem)
    extends BitcoinSRunner[Unit] {
  import ReadNegativeNumberResult.negativeNumberResultReads
  val filePath =
    Paths.get("/Users/chrisstewart/dev/bitcoin-s/negative-number-txs.json")
  val stream = FileIO
    .fromPath(filePath) // 8mb
    .via(
      Framing.delimiter(delimiter = ByteString("\n"),
                        maximumFrameLength = 300000,
                        allowTruncation = false)
    )
    .mapAsync(FutureUtil.getParallelism) { line =>
      Future {
        val json = Json.parse(line.utf8String)
        val c = json.validate[NegativeNumberResult].get
        c
      }
    } // Read line by line
  override def start(): Future[Unit] = {
    stream
      .runWith(Sink.foreach(println))
      .map(_ => ())
  }
  override def stop(): Future[Unit] = {
    system.terminate().map(_ => ())
  }

}

object ReadNegativeNumberResult extends BitcoinSAppScalaDaemon {
  override val actorSystemName: String =
    s"scan-bitcoind-${System.currentTimeMillis()}"

  override val customFinalDirOpt = None

  implicit val rpcAppConfig: BitcoindRpcAppConfig =
    BitcoindRpcAppConfig.fromDefaultDatadir()(using system)

  case class NegativeNumberResult(
      txid: DoubleSha256DigestBE,
      idx: Int,
      asm: String,
      negatives: String,
      `type`: String)

  import org.bitcoins.commons.serializers.JsonWriters.DoubleSha256DigestBEWrites
  import org.bitcoins.commons.serializers.JsonReaders.DoubleSha256DigestBEReads
  implicit lazy val negativeNumberResultWrites: Writes[NegativeNumberResult] =
    Json.writes[NegativeNumberResult]
  implicit lazy val negativeNumberResultReads: Reads[NegativeNumberResult] =
    Json.reads[NegativeNumberResult]

  new ReadNegativeNumberResult().run()
}
