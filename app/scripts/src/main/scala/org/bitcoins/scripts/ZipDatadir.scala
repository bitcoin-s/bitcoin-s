package org.bitcoins.scripts

import akka.actor.ActorSystem
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.server.routes.BitcoinSRunner
import org.bitcoins.server.util.BitcoinSApp

import java.nio.file.Paths
import scala.concurrent.Future

/** This script zips your $HOME/.bitcoin-s/ directory to a specified path, excluding chaindb.sqlite */
class ZipDatadir(override val args: Array[String])(implicit
    override val system: ActorSystem)
    extends BitcoinSRunner {

  implicit lazy val conf: BitcoinSAppConfig =
    BitcoinSAppConfig(datadir, baseConfig)

  override def start(): Future[Unit] = {

    //replace the line below with where you want to zip too
    val path = Paths.get("/tmp", "bitcoin-s.zip")
    val target = conf.zipDatadir(path)
    logger.info(s"Done zipping to $target!")
    for {
      _ <- system.terminate()
    } yield sys.exit(0)
  }

  override def stop(): Future[Unit] = Future.unit
}

object Zip extends BitcoinSApp {

  override val actorSystemName: String =
    s"zip-datadir-${System.currentTimeMillis()}"
  new ZipDatadir(args).run()
}
