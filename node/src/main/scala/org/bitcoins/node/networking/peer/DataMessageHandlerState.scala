package org.bitcoins.node.networking.peer

import org.bitcoins.node.models.Peer

import scala.collection.mutable

sealed abstract class DataMessageHandlerState

object DataMessageHandlerState {

  final case object HeaderSync extends DataMessageHandlerState

  case class ValidatingHeaders(
      inSyncWith: mutable.Set[Peer],
      failedCheck: mutable.Set[Peer],
      verifyingWith: Set[Peer]
  ) extends DataMessageHandlerState {
    def validated: Boolean = inSyncWith ++ failedCheck == verifyingWith
  }

  final case object PostHeaderSync extends DataMessageHandlerState
}
