package org.bitcoins.oracle.server

import org.bitcoins.dlc.oracle.config.DLCOracleAppConfig
import org.bitcoins.server.routes.{BitcoinSRunner, Server}

import scala.concurrent.Future

class OracleServerMain(override val args: Array[String])
    extends BitcoinSRunner {

  override val actorSystemName = "bitcoin-s-oracle"

  implicit val conf: DLCOracleAppConfig =
    DLCOracleAppConfig(datadir, baseConfig)

  override def start(): Future[Unit] = {

    val bindConfOpt = rpcBindOpt match {
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

object OracleServerMain extends App {
  new OracleServerMain(args).run(Some("oracle"))
}
