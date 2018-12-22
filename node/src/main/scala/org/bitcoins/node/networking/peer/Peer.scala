package org.bitcoins.node.networking.peer

import java.net.InetSocketAddress

import org.bitcoins.core.config.NetworkParameters

case class Peer(socket: InetSocketAddress, np: NetworkParameters)
