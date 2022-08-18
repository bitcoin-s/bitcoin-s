package org.bitcoins.node.networking.peer

import org.bitcoins.node.models.Peer

sealed abstract class DataMessageHandlerState

object DataMessageHandlerState {

  final case object HeaderSync extends DataMessageHandlerState

  case class ValidatingHeaders(
      inSyncWith: Set[Peer],
      failedCheck: Set[Peer],
      verifyingWith: Set[Peer]
  ) extends DataMessageHandlerState {
    def validated: Boolean = inSyncWith ++ failedCheck == verifyingWith
  }

  final case object PostHeaderSync extends DataMessageHandlerState
}
