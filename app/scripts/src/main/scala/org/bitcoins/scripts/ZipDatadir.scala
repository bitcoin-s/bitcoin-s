package org.bitcoins.scripts

import akka.actor.ActorSystem
import org.bitcoins.commons.util.{DatadirParser, ServerArgParser}
import org.bitcoins.db.DatadirUtil
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.server.routes.BitcoinSServerRunner
import org.bitcoins.server.util.BitcoinSAppScalaDaemon

import java.nio.file.Paths
import scala.concurrent.Future

/** This script zips your $HOME/.bitcoin-s/ directory to a specified path, excluding chaindb.sqlite */
class ZipDatadir(override val serverArgParser: ServerArgParser)(implicit
    override val system: ActorSystem,
    conf: BitcoinSAppConfig)
    extends BitcoinSServerRunner {

  override def start(): Future[Unit] = {

    //replace the line below with where you want to zip too
    val path = Paths.get("/tmp", "bitcoin-s.zip")
    val target = DatadirUtil.zipDatadir(conf.baseDatadir, path)
    logger.info(s"Done zipping to $target!")
    for {
      _ <- system.terminate()
    } yield sys.exit(0)
  }

  override def stop(): Future[Unit] = Future.unit
}

object Zip extends BitcoinSAppScalaDaemon {

  override val actorSystemName: String =
    s"zip-datadir-${System.currentTimeMillis()}"

  override val customFinalDirOpt = None

  val serverCmdLineArgs = ServerArgParser(args.toVector)

  val datadirParser =
    DatadirParser(serverCmdLineArgs, customFinalDirOpt)

  System.setProperty("bitcoins.log.location", datadirParser.networkDir.toString)

  implicit lazy val conf: BitcoinSAppConfig =
    BitcoinSAppConfig(datadirParser.datadir, Vector(datadirParser.baseConfig))(
      system)

  new ZipDatadir(serverCmdLineArgs).run()
}
