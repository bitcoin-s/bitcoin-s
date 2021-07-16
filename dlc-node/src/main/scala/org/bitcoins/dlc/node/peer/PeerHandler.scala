package org.bitcoins.dlc.node.peer

import org.bitcoins.dlc.node.P2PClient

case class PeerHandler(p2pClient: P2PClient, peerMsgSender: PeerMessageSender)
