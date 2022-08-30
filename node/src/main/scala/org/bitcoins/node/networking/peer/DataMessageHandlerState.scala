package org.bitcoins.node.networking.peer

import org.bitcoins.core.api.chain.db.CompactFilterHeaderDb
import org.bitcoins.core.p2p.{
  GetCompactFilterHeadersMessage,
  GetCompactFiltersMessage
}
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

  final case object FetchFilterCheckpoints extends DataMessageHandlerState

  case class FilterHeaderSync(
      nextFetchHeight: Int,
      verifyLater: Vector[(CompactFilterHeaderDb, CompactFilterHeaderDb)],
      failedQueries: Vector[GetCompactFilterHeadersMessage],
      askedFor: Map[Peer, Int],
      headerCount: Int)
      extends DataMessageHandlerState

  case class FilterSync(
      nextFetchHeight: Int,
      askedFor: Map[Peer, Int],
      failedQueries: Vector[GetCompactFiltersMessage])
      extends DataMessageHandlerState

  final case object IBDDone extends DataMessageHandlerState
}
