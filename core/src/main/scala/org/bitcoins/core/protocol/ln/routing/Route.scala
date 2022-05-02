package org.bitcoins.core.protocol.ln.routing

import org.bitcoins.core.protocol.ln.channel.ShortChannelId
import org.bitcoins.core.protocol.ln.currency.MilliSatoshis
import org.bitcoins.core.protocol.ln.node.NodeId

/** Represent differet types of LN routes. Supports node and channel routes.
  */
sealed trait Route {
  val amount: MilliSatoshis
}

case class NodeRoute(
    override val amount: MilliSatoshis,
    nodeIds: Vector[NodeId])
    extends Route

case class ChannelRoute(
    override val amount: MilliSatoshis,
    shortChannelIds: Vector[ShortChannelId])
    extends Route
