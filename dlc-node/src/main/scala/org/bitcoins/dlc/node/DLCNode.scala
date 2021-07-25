package org.bitcoins.dlc.node

import akka.actor.{ActorRef, ActorSystem}
import grizzled.slf4j.Logging
import org.bitcoins.core.api.dlc.wallet.DLCWalletApi
import org.bitcoins.core.util.StartStopAsync
import org.bitcoins.dlc.node.config._

import scala.concurrent._

case class DLCNode(wallet: DLCWalletApi)(implicit
    system: ActorSystem,
    config: DLCNodeAppConfig)
    extends StartStopAsync[Unit]
    with Logging {

  implicit val ec: ExecutionContextExecutor = system.dispatcher

  private val serverActor: ActorRef =
    system.actorOf(DLCServer.props(wallet, config.listenAddress))

  override def start(): Future[Unit] = Future.unit

  override def stop(): Future[Unit] = {
    system.stop(serverActor)
    Future.unit
  }
}
