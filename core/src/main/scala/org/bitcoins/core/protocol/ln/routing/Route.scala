package org.bitcoins.core.protocol.ln.routing

import org.bitcoins.core.protocol.ln.channel.ShortChannelId
import org.bitcoins.core.protocol.ln.node.NodeId

/**
  * Represent differet types of LN routes. Supports node and channel routes.
  */
sealed trait Route

case class NodeRoute(ids: Vector[NodeId]) extends Route

case class ChannelRoute(ids: Vector[ShortChannelId]) extends Route
