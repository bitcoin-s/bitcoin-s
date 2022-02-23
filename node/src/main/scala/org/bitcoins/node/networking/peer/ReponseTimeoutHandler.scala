//package org.bitcoins.node.networking.peer
//
//import org.bitcoins.core.p2p.{ExpectsResponse, GetHeadersMessage, VersionMessage}
//import org.bitcoins.node.Node
//
//case class ReponseTimeoutHandler(node: Node) {
//
//  def handleResponseTimeout(msg: ExpectsResponse): Unit ={
//    msg match {
//      case message: GetHeadersMessage => ???
//      case message: VersionMessage =>
//        //could not initialize peer, disconnect
//        node.peerManager.onInitializationTimeout(peer)
//    }
//  }
//}
