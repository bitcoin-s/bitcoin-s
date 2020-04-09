package org.bitcoins.commons.jsonmodels.eclair

sealed abstract class PeerState

object PeerState {
  case object CONNECTED extends PeerState

  case object INITIALIZING extends PeerState

  case object DISCONNECTED extends PeerState

  private val all = List(CONNECTED, INITIALIZING, DISCONNECTED)

  def fromString(str: String): Option[PeerState] = {
    all.find(_.toString == str)
  }
}
