package org.bitcoins.scripts

import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.server.routes.BitcoinSRunner

import java.nio.file.Paths
import scala.concurrent.Future

/** This script zips your $HOME/.bitcoin-s/ directory to a specified path, excluding chaindb.sqlite */
class ZipDatadir(override val args: Array[String]) extends BitcoinSRunner {

  override def actorSystemName: String = "Zip-datadir"

  implicit lazy val conf: BitcoinSAppConfig =
    BitcoinSAppConfig(datadir, baseConfig)

  override def startup: Future[Unit] = {

    //replace the line below with where you want to zip too
    val path = Paths.get("/tmp", "bitcoin-s.zip")
    val target = conf.zipDatadir(path)
    logger.info(s"Done zipping to $target!")
    for {
      _ <- system.terminate()
    } yield sys.exit(0)
  }
}

object Zip extends App {
  new ZipDatadir(args).run()
}
