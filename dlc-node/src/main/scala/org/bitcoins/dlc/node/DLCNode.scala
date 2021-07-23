package org.bitcoins.dlc.node

import akka.actor.ActorSystem
import grizzled.slf4j.Logging
import org.bitcoins.core.api.dlc.wallet.DLCWalletApi
import org.bitcoins.core.util.StartStopAsync
import org.bitcoins.dlc.node.config._

import java.net.InetSocketAddress
import scala.concurrent._

case class DLCNode(wallet: DLCWalletApi)(implicit
    system: ActorSystem,
    config: DLCNodeAppConfig)
    extends StartStopAsync[Unit]
    with Logging {

  implicit val ec: ExecutionContextExecutor = system.dispatcher

  lazy val serverBindF: Future[InetSocketAddress] = DLCServer.bind(
    wallet,
    config.listenAddress,
    None // todo Tor params in config
  )

  override def start(): Future[Unit] = serverBindF.map(_ => ())

  override def stop(): Future[Unit] = {
    Future.unit
  }
}
