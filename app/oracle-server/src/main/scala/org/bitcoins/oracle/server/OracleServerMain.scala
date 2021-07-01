package org.bitcoins.oracle.server

import akka.actor.ActorSystem
import org.bitcoins.db.util.DatadirParser
import org.bitcoins.dlc.oracle.config.DLCOracleAppConfig
import org.bitcoins.server.routes.{BitcoinSRunner, Server}
import org.bitcoins.server.util.{BitcoinSApp, BitcoinSAppScalaDaemon}

import scala.concurrent.Future

class OracleServerMain(override val args: Array[String])(implicit
    override val system: ActorSystem,
    conf: DLCOracleAppConfig)
    extends BitcoinSRunner {

  override def start(): Future[Unit] = {

    val bindConfOpt = conf.rpcBindOpt match {
      case Some(rpcbind) => Some(rpcbind)
      case None          => conf.rpcBindOpt
    }

    for {
      _ <- conf.start()
      oracle <- conf.initialize()

      routes = Seq(OracleRoutes(oracle))
      server = rpcPortOpt match {
        case Some(rpcport) =>
          Server(conf = conf,
                 handlers = routes,
                 rpcbindOpt = bindConfOpt,
                 rpcport = rpcport)
        case None =>
          Server(conf = conf,
                 handlers = routes,
                 rpcbindOpt = bindConfOpt,
                 rpcport = conf.rpcPort)
      }

      _ <- server.start()
    } yield {
      logger.info(s"Done starting oracle!")
      ()
    }
  }

  override def stop(): Future[Unit] = {
    logger.error(s"Exiting process")
    for {
      _ <- conf.stop()
      _ = logger.info(s"Stopped DLC Oracle")
      _ <- system.terminate()
    } yield {
      logger.info(s"Actor system terminated")
      ()
    }
  }
}

object OracleServerMain extends BitcoinSAppScalaDaemon {

  override val actorSystemName =
    s"bitcoin-s-oracle-${System.currentTimeMillis()}"

  /** Directory specific for current network or custom dir */
  override val customFinalDirOpt: Option[String] = Some("oracle")

  val datadirParser =
    DatadirParser(args.toVector, networkOpt, customFinalDirOpt)

  System.setProperty("bitcoins.log.location", datadirParser.usedDir.toString)

  implicit lazy val conf: DLCOracleAppConfig =
    DLCOracleAppConfig(datadirParser.datadir, datadirParser.baseConfig)(
      system.dispatcher)

  new OracleServerMain(args).run()
}
