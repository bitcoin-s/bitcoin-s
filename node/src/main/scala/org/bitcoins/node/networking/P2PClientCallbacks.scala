package org.bitcoins.node.networking

import org.bitcoins.core.p2p.{ExpectsResponse, NetworkPayload}
import org.bitcoins.node.models.Peer

import scala.concurrent.Future

case class P2PClientCallbacks(
    onReconnect: Peer => Future[Unit],
    onDisconnect: (Peer, Boolean) => Future[Unit],
    onInitializationTimeout: Peer => Future[Unit],
    onQueryTimeout: (ExpectsResponse, Peer) => Future[Unit],
    sendResponseTimeout: (Peer, NetworkPayload) => Future[Unit]
)

object P2PClientCallbacks {

  def empty: P2PClientCallbacks = {
    P2PClientCallbacks(
      onReconnect = (_: Peer) => Future.unit,
      onDisconnect = (_: Peer, _: Boolean) => Future.unit,
      onInitializationTimeout = (_: Peer) => Future.unit,
      onQueryTimeout = (_, _) => Future.unit,
      sendResponseTimeout = (_, _) => Future.unit
    )
  }
}
