package org.bitcoins.dlc.node.peer

import org.bitcoins.core.protocol.tlv._
import org.bitcoins.dlc.node.P2PClient

import scala.concurrent.{Future, Promise}

sealed abstract class PeerMessageReceiverState {

  /** This promise gets completed when we receive a
    * [[akka.io.Tcp.Connected]] message from [[org.bitcoins.dlc.node.P2PClient P2PClient]]
    */
  def clientConnectP: Promise[P2PClient]

  /** The [[org.bitcoins.dlc.node.P2PClient P2PClient]] we are
    * connected to. This isn't initiated until the client
    * has called [[org.bitcoins.dlc.node.peer.PeerMessageReceiver.connect() connect()]]
    */
  private val clientConnectF: Future[P2PClient] = clientConnectP.future

  /** This promise is completed in the [[org.bitcoins.dlc.node.peer.PeerMessageReceiver.disconnect() disconnect()]]
    * when a [[org.bitcoins.dlc.node.P2PClient P2PClient]] initiates a disconnections from
    * our peer on the p2p network
    */
  def clientDisconnectP: Promise[Unit]

  private val clientDisconnectF: Future[Unit] = clientDisconnectP.future

  /** If this future is completed, we are
    * connected to our client. Note, there is
    * no timeout on this future and no guarantee
    * that some one has actually initiated
    * a connection with a [[org.bitcoins.dlc.node.P2PClient P2PClient]]
    * @return
    */
  def isConnected: Boolean = {
    clientConnectF.isCompleted && !clientDisconnectF.isCompleted
  }

  def isDisconnected: Boolean = {
    clientDisconnectF.isCompleted && !isConnected
  }

  def initMessageP: Promise[LnMessage[InitTLV]]

  /** This future is completed when our peer has sent
    * us their [[org.bitcoins.core.p2p.VersionMessage VersionMessage]] indicating what protocol
    * features they support
    */
  def hasInitMessageReceived: Future[LnMessage[InitTLV]] = {
    initMessageP.future
  }

  /** Indicates we have connected and completed the initial
    * handshake that is required to connect to the bitcoin p2p network
    * If this is true, we can start sending and receiving normal
    * [[org.bitcoins.core.p2p.NetworkMessage NetworkMessage]] with our peer on the network
    * @return
    */
  def isInitialized: Boolean = {
    hasInitMessageReceived.isCompleted
  }
}

object PeerMessageReceiverState {

  /** Represents a [[org.bitcoins.dlc.node.peer.PeerMessageReceiverState PeerMessageReceiverState]]
    * where the peer is not connected to the p2p network
    */
  final case object Preconnection extends PeerMessageReceiverState {
    def clientConnectP: Promise[P2PClient] = Promise[P2PClient]()

    //should this be completed since the client is disconnected???
    def clientDisconnectP: Promise[Unit] = Promise[Unit]()

    def initMessageP: Promise[LnMessage[InitTLV]] =
      Promise[LnMessage[InitTLV]]()

    /** Converts [[org.bitcoins.dlc.node.peer.PeerMessageReceiverState.Preconnection Preconnection]] to [[Initializing]] */
    def toInitializing(client: P2PClient): Initializing = {
      val p = clientConnectP
      p.success(client)
      Initializing(
        clientConnectP = p,
        clientDisconnectP = clientDisconnectP,
        initMessageP = initMessageP
      )
    }
  }

  /** Means that our [[org.bitcoins.dlc.node.peer.PeerMessageReceiver]]
    * is still going through the initialization process. This means
    * we still need to receive a [[org.bitcoins.core.p2p.VersionMessage VersionMessage]] or [[org.bitcoins.core.p2p.VerAckMessage VerAckMessage]]
    * from our peer on the p2p network
    */
  case class Initializing(
      clientConnectP: Promise[P2PClient],
      clientDisconnectP: Promise[Unit],
      initMessageP: Promise[LnMessage[InitTLV]]
  ) extends PeerMessageReceiverState {
    require(
      isConnected,
      "We cannot have a PeerMessageReceiverState.Initializing if we are not connected")

    /** Helper method to modifing the state of [[org.bitcoins.dlc.node.peer.PeerMessageReceiverState.Initializing]]
      * when we receive a [[org.bitcoins.core.p2p.VersionMessage VersionMessage]]. This completes versoinMsgP
      * @return
      */
    def withVersionMsg(initMessage: LnMessage[InitTLV]): Initializing = {
      PeerMessageReceiverState.Initializing(
        clientConnectP = clientConnectP,
        clientDisconnectP = clientDisconnectP,
        initMessageP = initMessageP.success(initMessage)
      )
    }

    /** Completes the verack message promise and transitions
      * our [[org.bitcoins.dlc.node.peer.PeerMessageReceiverState PeerMessageReceiverState]] to [[org.bitcoins.dlc.node.peer.PeerMessageReceiverState.Normal PeerMessageReceiverState.Normal]]
      */
    def toNormal: Normal = {
      Normal(
        clientConnectP = clientConnectP,
        clientDisconnectP = clientDisconnectP,
        initMessageP = initMessageP
      )
    }

    override def toString: String = "Initializing"
  }

  /** This represents a [[org.bitcoins.dlc.node.peer.PeerMessageReceiverState]]
    * where the peer has been fully initialized and is ready to send messages to
    * the peer on the network
    */
  case class Normal(
      clientConnectP: Promise[P2PClient],
      clientDisconnectP: Promise[Unit],
      initMessageP: Promise[LnMessage[InitTLV]]
  ) extends PeerMessageReceiverState {
    require(
      isConnected,
      s"We cannot have a PeerMessageReceiverState.Normal if the Peer is not connected")
    require(
      isInitialized,
      s"We cannot have a PeerMessageReceiverState.Normal if the Peer is not initialized")

    override def toString: String = "Normal"
  }

  case class Disconnected(
      clientConnectP: Promise[P2PClient],
      clientDisconnectP: Promise[Unit],
      initMessageP: Promise[LnMessage[InitTLV]])
      extends PeerMessageReceiverState {
    require(
      isDisconnected,
      "We cannot be in the disconnected state if a peer is not disconnected")

    override def toString: String = "Disconnected"

  }

  def fresh(): PeerMessageReceiverState.Preconnection.type = {
    PeerMessageReceiverState.Preconnection
  }

}
