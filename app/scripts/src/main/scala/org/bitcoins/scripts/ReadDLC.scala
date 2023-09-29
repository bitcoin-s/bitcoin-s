package org.bitcoins.scripts

import akka.actor.ActorSystem
import org.bitcoins.commons.util.{DatadirParser, ServerArgParser}
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.dlc.wallet.models.{DLCContractDataDAO, DLCDAO}
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.server.routes.BitcoinSServerRunner
import org.bitcoins.server.util.BitcoinSAppScalaDaemon

import scala.concurrent.Future

case class ReadDLC(override val serverArgParser: ServerArgParser)(implicit
    override val system: ActorSystem,
    conf: BitcoinSAppConfig)
    extends BitcoinSServerRunner[Unit] {

  override def start(): Future[Unit] = {
    val dlcDAO = DLCDAO()(system.dispatcher, conf.dlcConf)
    val contractDAO = DLCContractDataDAO()(system.dispatcher, conf.dlcConf)
    val dlcIdsF = dlcDAO.findAll().map(_.map(_.dlcId))
    //df208c05cd9204b2ce4375a2d2c0384fd777f65f7d8338d0e9ffcacd58c30468
    for {
      dlcIds <- dlcIdsF
      _ <- FutureUtil.sequentially(dlcIds) { dlcId =>
        val contractF = contractDAO.findByDLCId(dlcId)
        contractF.failed.foreach(err =>
          logger.error(s"Failed to parse dlcId=$dlcId", err))
        contractF
      }
    } yield ()
  }

  override def stop(): Future[Unit] = Future.unit

}

object ReadDLC extends BitcoinSAppScalaDaemon {

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

  new ReadDLC(serverCmdLineArgs).run()
}
