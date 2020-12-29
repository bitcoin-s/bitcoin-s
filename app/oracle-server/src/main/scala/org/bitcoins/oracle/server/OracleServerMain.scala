package org.bitcoins.oracle.server

import org.bitcoins.dlc.oracle.config.DLCOracleAppConfig
import org.bitcoins.server.routes.{BitcoinSRunner, Server}

import scala.concurrent.Future

class OracleServerMain(override val args: Array[String])
    extends BitcoinSRunner {

  override val actorSystemName = "bitcoin-s-oracle"

  override def startup: Future[Unit] = {

    implicit val conf: DLCOracleAppConfig =
      DLCOracleAppConfig(datadir, baseConfig)

    for {
      _ <- conf.start()
      oracle <- conf.initialize()

      routes = Seq(OracleRoutes(oracle))
      server = rpcPortOpt match {
        case Some(rpcport) =>
          Server(conf = conf,
                 handlers = routes,
                 rpcbindOpt = rpcBindOpt,
                 rpcport = rpcport)
        case None =>
          conf.rpcPortOpt match {
            case Some(rpcport) =>
              Server(conf = conf,
                     handlers = routes,
                     rpcbindOpt = rpcBindOpt,
                     rpcport = rpcport)
            case None =>
              Server(conf = conf, handlers = routes, rpcbindOpt = rpcBindOpt)
          }
      }

      _ <- server.start()
    } yield {
      logger.info(s"Done starting oracle!")
      sys.addShutdownHook {
        logger.error(s"Exiting process")

        conf.stop().foreach(_ => logger.info(s"Stopped DLC Oracle"))
        system.terminate().foreach(_ => logger.info(s"Actor system terminated"))
      }
      ()
    }
  }
}

object OracleServerMain extends App {
  new OracleServerMain(args).run()
}
