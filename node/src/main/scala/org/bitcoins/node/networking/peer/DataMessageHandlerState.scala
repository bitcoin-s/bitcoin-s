package org.bitcoins.node.networking.peer

import grizzled.slf4j.Logging
import org.bitcoins.node.models.Peer

sealed trait DataMessageHandlerState extends Logging

object DataMessageHandlerState {

  final case object Initial extends DataMessageHandlerState {
    override def toString: String = "Initial"
  }

  case class HeaderSync(
      syncPeer: Peer
  ) extends DataMessageHandlerState {
    override def toString: String = "HeaderSync"
  }

  case class ValidatingHeaders(
      inSyncWith: Set[Peer]
  ) extends DataMessageHandlerState {
    override def toString: String = "ValidatingHeader"
  }

  case class FilterHeaderSync(
      syncPeer: Peer
  ) extends DataMessageHandlerState {
    override def toString: String = "FilterHeaderSync"
  }

  case class FilterSync(
      syncPeer: Peer
  ) extends DataMessageHandlerState {
    override def toString: String = "FilterSync"
  }

  case class Normal(
      syncPeer: Peer
  ) extends DataMessageHandlerState {
    override def toString: String = "Normal"
  }

}
