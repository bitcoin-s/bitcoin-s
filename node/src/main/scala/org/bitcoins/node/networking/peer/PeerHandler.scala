package org.bitcoins.node.networking.peer

import org.bitcoins.node.networking.P2PClient

case class PeerHandler(p2pClient: P2PClient, peerMsgSender: PeerMessageSender)
