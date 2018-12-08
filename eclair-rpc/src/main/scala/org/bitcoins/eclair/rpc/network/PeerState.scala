package org.bitcoins.eclair.rpc.network

sealed abstract class PeerState

object PeerState {
  case object CONNECTED extends PeerState

  case object DISCONNECTED extends PeerState

  private val all = List(CONNECTED, DISCONNECTED)

  def fromString(str: String): Option[PeerState] = {
    all.find(_.toString == str)
  }
}