package org.bitcoins.node.networking.peer

import org.bitcoins.core.p2p.{VerAckMessage, VersionMessage}
import org.bitcoins.node.networking.P2PClient

import scala.concurrent.{Future, Promise}

sealed abstract class PeerMessageReceiverState {

  /** This promise gets completed when we receive a
    * [[akka.io.Tcp.Connected]] message from [[org.bitcoins.node.networking.P2PClient P2PClient]]
    */
  def clientConnectP: Promise[P2PClient]

  /** The [[org.bitcoins.node.networking.P2PClient P2PClient]] we are
    * connected to. This isn't initiated until the client
    * has called [[org.bitcoins.node.networking.peer.PeerMessageReceiver.connect() connect()]]
    */
  private val clientConnectF: Future[P2PClient] = clientConnectP.future

  /** This promise is completed in the [[org.bitcoins.node.networking.peer.PeerMessageReceiver.disconnect() disconnect()]]
    * when a [[org.bitcoins.node.networking.P2PClient P2PClient]] initiates a disconnections from
    * our peer on the p2p network
    */
  def clientDisconnectP: Promise[Unit]

  private val clientDisconnectF: Future[Unit] = clientDisconnectP.future

  /** If this future is completed, we are
    * connected to our client. Note, there is
    * no timeout on this future and no guarantee
    * that some one has actually initiated
    * a connection with a [[org.bitcoins.node.networking.P2PClient P2PClient]]
    * @return
    */
  def isConnected: Boolean = {
    clientConnectF.isCompleted && !clientDisconnectF.isCompleted
  }

  def isDisconnected: Boolean = {
    clientDisconnectF.isCompleted && !isConnected
  }

  def versionMsgP: Promise[VersionMessage]

  /** This future is completed when our peer has sent
    * us their [[org.bitcoins.core.p2p.VersionMessage VersionMessage]] indicating what protocol
    * features they support
    */
  def hasReceivedVersionMsg: Future[VersionMessage] = {
    versionMsgP.future
  }

  def verackMsgP: Promise[VerAckMessage.type]

  /** This future completes when we have received a
    * [[org.bitcoins.core.p2p.VerAckMessage VerAckMessage]] from our peer. This means our
    * peer has accepted our [[org.bitcoins.core.p2p.VersionMessage VersionMessage]] and is
    * willing to connect with us
    * @return
    */
  def hasReceivedVerackMsg: Future[VerAckMessage.type] = {
    verackMsgP.future
  }

  /** Indicates we have connected and completed the initial
    * handshake that is required to connect to the bitcoin p2p network
    * If this is true, we can start sending and receiving normal
    * [[org.bitcoins.core.p2p.NetworkMessage NetworkMessage]] with our peer on the network
    * @return
    */
  def isInitialized: Boolean = {
    hasReceivedVersionMsg.isCompleted && hasReceivedVerackMsg.isCompleted
  }
}

object PeerMessageReceiverState {

  /** Represents a [[org.bitcoins.node.networking.peer.PeerMessageReceiverState PeerMessageReceiverState]]
    * where the peer is not connected to the p2p network
    */
  final case object Preconnection extends PeerMessageReceiverState {
    def clientConnectP: Promise[P2PClient] = Promise[P2PClient]()

    //should this be completed since the client is disconnected???
    def clientDisconnectP: Promise[Unit] = Promise[Unit]()
    def versionMsgP: Promise[VersionMessage] = Promise[VersionMessage]()
    def verackMsgP: Promise[VerAckMessage.type] = Promise[VerAckMessage.type]()

    /** Converts [[org.bitcoins.node.networking.peer.PeerMessageReceiverState.Preconnection Preconnection]] to [[Initializing]] */
    def toInitializing(client: P2PClient): Initializing = {
      val p = clientConnectP
      p.success(client)
      Initializing(
        clientConnectP = p,
        clientDisconnectP = clientDisconnectP,
        versionMsgP = versionMsgP,
        verackMsgP = verackMsgP
      )
    }
  }

  /** Means that our [[org.bitcoins.node.networking.peer.PeerMessageReceiver]]
    * is still going through the initilization process. This means
    * we still need to receive a [[org.bitcoins.core.p2p.VersionMessage VersionMessage]] or [[org.bitcoins.core.p2p.VerAckMessage VerAckMessage]]
    * from our peer on the p2p network
    */
  case class Initializing(
      clientConnectP: Promise[P2PClient],
      clientDisconnectP: Promise[Unit],
      versionMsgP: Promise[VersionMessage],
      verackMsgP: Promise[VerAckMessage.type]
  ) extends PeerMessageReceiverState {
    require(
      isConnected,
      "We cannot have a PeerMessageReceiverState.Initializng if we are not connected")

    /** Helper method to modifing the state of [[org.bitcoins.node.networking.peer.PeerMessageReceiverState.Initializing]]
      * when we receive a [[org.bitcoins.core.p2p.VersionMessage VersionMessage]]. This completes versoinMsgP
      * @return
      */
    def withVersionMsg(versionMsg: VersionMessage): Initializing = {
      PeerMessageReceiverState.Initializing(
        clientConnectP = clientConnectP,
        clientDisconnectP = clientDisconnectP,
        versionMsgP = versionMsgP.success(versionMsg),
        verackMsgP = verackMsgP
      )
    }

    /** Completes the verack message promise and transitions
      * our [[org.bitcoins.node.networking.peer.PeerMessageReceiverState PeerMessageReceiverState]] to [[org.bitcoins.node.networking.peer.PeerMessageReceiverState.Normal PeerMessageReceiverState.Normal]]
      */
    def toNormal(verAckMessage: VerAckMessage.type): Normal = {
      Normal(
        clientConnectP = clientConnectP,
        clientDisconnectP = clientDisconnectP,
        versionMsgP = versionMsgP,
        verackMsgP = verackMsgP.success(verAckMessage)
      )
    }

    override def toString: String = "Initializing"
  }

  /** This represents a [[org.bitcoins.node.networking.peer.PeerMessageReceiverState]]
    * where the peer has been fully initialized and is ready to send messages to
    * the peer on the network
    */
  case class Normal(
      clientConnectP: Promise[P2PClient],
      clientDisconnectP: Promise[Unit],
      versionMsgP: Promise[VersionMessage],
      verackMsgP: Promise[VerAckMessage.type]
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
      versionMsgP: Promise[VersionMessage],
      verackMsgP: Promise[VerAckMessage.type])
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
