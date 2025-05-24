package org.bitcoins.scripts

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.stream.scaladsl.{Keep, Sink}
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.util.{EnvUtil, FutureUtil}
import org.bitcoins.rpc.config.BitcoindRpcAppConfig
import org.bitcoins.server.routes.BitcoinSRunner
import org.bitcoins.server.util.BitcoinSAppScalaDaemon

import java.nio.file.{Files, Path, Paths}
import java.time.{Duration, Instant}
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.Future

case class CheckP2SHDb()(override implicit val system: ActorSystem)
    extends BitcoinSRunner[Unit] {
  import slick.jdbc.SQLiteProfile
  import slick.jdbc.SQLiteProfile.api.{
    BaseColumnType,
    Database,
    MappedColumnType,
    Table,
    TableQuery,
    Tag,
    intColumnType,
    stringColumnType
  }
  import slick.lifted.BaseColumnExtensionMethods

  // Define your table schema (adjust data types as needed)
  class Utxos(tag: Tag)
      extends Table[(String, Int, Int, Int, Int, ScriptPubKey)](tag, "utxos") {
    val db = new org.bitcoins.db.DbCommonsColumnMappers(SQLiteProfile)
    implicit val scriptPubKeyAsmMapper: BaseColumnType[ScriptPubKey] = {
      MappedColumnType.base[ScriptPubKey, String](_.asmHex,
                                                  ScriptPubKey.fromAsmHex)
    }
    def txid = column[String]("txid")
    def vout = column[Int]("vout")
    def value = column[Int]("value")
    def coinbase = column[Int]("coinbase")
    def height = column[Int]("height")
    def scriptpubkey = column[ScriptPubKey]("scriptpubkey")

    def * = (txid, vout, value, coinbase, height, scriptpubkey)
  }

  implicit val scriptPubKeyAsmMapper: BaseColumnType[ScriptPubKey] = {
    MappedColumnType.base[ScriptPubKey, String](_.asmHex,
                                                ScriptPubKey.fromAsmHex)
  }

  val filePath: Path = {
    if (EnvUtil.isMac) {
      Paths.get("/Users/chrisstewart/dev/bitcoin-s/p2sh-size.json")
    } else {
      // Paths.get("p2sh-2.json") x
      Paths.get("p2sh-3.json")
    }
  }
  val lineCountF = Future {
    Files.lines(filePath).count()
  } // Uses Java Streams API
  val dbPath = if (EnvUtil.isMac) {
    "jdbc:sqlite:/Users/chrisstewart/dev/bitcoin-s/2025-02-19-utxo.sqlite"
  } else {
    "jdbc:sqlite:/home/chris/dev/bitcoin-s/2025-02-19-utxo.sqlite"
  }

  override def start(): Future[Unit] = {
    val db = Database.forURL(
      dbPath,
      driver = "org.sqlite.JDBC"
    ) // Use the correct JDBC driver
    val utxos = TableQuery[Utxos]
    val start = Instant.now()
    val grouping = 15_000
    val drop = 0
    val count = new AtomicInteger(drop)
    val fmt = { (combo: Combo) => combo.spk }

    val parallelism = FutureUtil.getParallelism
    val source =
      CheckP2SH.getP2SHSource(filePath, fmt, grouping, drop, parallelism)

    val resultF: Future[Vector[(String, Int, Int, Int, Int, ScriptPubKey)]] = {
      def runStream(lineCountOpt: Option[Long], spks: Vector[ScriptPubKey])
          : Future[Vector[(String, Int, Int, Int, Int, ScriptPubKey)]] = {
        val lineCount = lineCountOpt.getOrElse(Long.MaxValue)
        val percent = (count.get().toDouble / lineCount) * 100.0
        val totalTime = Duration.between(start, Instant.now())

        if (count.get() % grouping == 0) {
          logger.info(
            s"Searching $count addresses " + f"$percent%.2f" + s"% total time=$totalTime batchSize=${spks.length}")
        }
        import slick.jdbc.SQLiteProfile.api.streamableQueryActionExtensionMethods
        val query = utxos.filter(u =>
          new BaseColumnExtensionMethods(u.scriptpubkey).inSet(spks))
        count.addAndGet(spks.length)
        db.run(query.result).map(_.toVector)
      }

      source
        .mapAsync(parallelism) { spks =>
          if (lineCountF.isCompleted) {
            lineCountF.flatMap { lineCount =>
              runStream(Some(lineCount), spks)
            }
          } else {
            runStream(None, spks)
          }
        }
        .filter(_.nonEmpty)
        .toMat(Sink.seq)(Keep.right)
        .run()
        .map(_.flatten.toVector)
    }

    resultF.map { results =>
      logger.info(
        s"Found a total of ${results.length} spks in the utxo set in file=${filePath.toAbsolutePath}, it took=${Duration
            .between(start, Instant.now())}")
      results.foreach(r => logger.info(s"result=$r"))
      ()
    }
  }
  override def stop(): Future[Unit] = system.terminate().map(_ => ())
}

object CheckP2SHDb extends BitcoinSAppScalaDaemon {

  override val actorSystemName: String =
    s"check-p2sh-${System.currentTimeMillis()}"

  override val customFinalDirOpt = None

  implicit val rpcAppConfig: BitcoindRpcAppConfig =
    BitcoindRpcAppConfig.fromDefaultDatadir()(system)

  new CheckP2SHDb().run()
}
