package org.bitcoins.dlc.node

import akka.io.Tcp
import akka.testkit._
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.dlc.node.P2PClient._
import org.bitcoins.dlc.node.peer._
import org.bitcoins.testkit.async.TestAsyncUtil
import org.bitcoins.testkit.keymanager.KeyManagerTestUtil.bip39PasswordOpt
import org.bitcoins.testkit.node._
import org.bitcoins.testkit.util.BitcoinSAsyncTest
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.testkit.wallet.BitcoinSWalletTest.{
  MockChainQueryApi,
  MockNodeApi
}
import org.bitcoins.testkitcore.Implicits.GeneratorOps
import org.bitcoins.testkitcore.gen.LnMessageGen
import org.scalatest.Assertion

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

class P2PClientTest extends BitcoinSAsyncTest with CachedBitcoinSAppConfig {

  behavior of "parseIndividualMessages"

  it must "DLC Accept message that is not aligned with a tcp frame" in {
    val accept: LnMessage[DLCAcceptTLV] =
      LnMessageGen.dlcAcceptMessage.sampleSome

    //split the msg at a random index to simulate a tcp frame not being aligned
    val randomIndex = scala.util.Random.nextInt().abs % accept.bytes.size
    val (firstHalf, secondHalf) = accept.bytes.splitAt(randomIndex)
    val (firstHalfParseHeaders, remainingBytes) =
      P2PClient.parseIndividualMessages(firstHalf)
    firstHalfParseHeaders must be(empty)

    val (secondHalfParsedHeaders, _) =
      P2PClient.parseIndividualMessages(remainingBytes ++ secondHalf)
    val parsedLnMessage = secondHalfParsedHeaders.head
    val parsedLnAcceptMessage =
      parsedLnMessage.asInstanceOf[LnMessage[DLCAcceptTLV]]

    parsedLnAcceptMessage.bytes must be(accept.bytes)
  }

  it must "return the entire byte array if a message is not aligned to a byte frame" in {
    val message = LnMessageGen.knownLnMessage.sampleSome
    // remove last byte so the message is not aligned
    val bytes = message.bytes.dropRight(1)
    val (parsedMessages, unAlignedBytes) =
      P2PClient.parseIndividualMessages(bytes)

    assert(parsedMessages.isEmpty)
    assert(unAlignedBytes == bytes)
  }

  // todo figure out how to properly handle unknown messages
  it must "parse an unknown message" ignore {
    val unknown = LnMessageGen.unknownMessage.sampleSome

    val (messages, leftover) = P2PClient.parseIndividualMessages(unknown.bytes)
    assert(messages == Vector(unknown))
    assert(leftover.isEmpty)
  }

  behavior of "P2PClient"

//  it must "establish a tcp connection between 2 nodes" in {
//    bitcoindPeerF.flatMap { remote =>
//      connectAndDisconnect(remote)
//    }
//  }

//  it must "connect to two nodes" in {
//    val try1 =
//      bitcoindPeerF.flatMap(remote => connectAndDisconnect(remote))
//
//    val try2 = bitcoindPeer2F.flatMap(remote => connectAndDisconnect(remote))
//
//    try1.flatMap { _ =>
//      try2
//    }
//  }

  /** Helper method to connect to the
    * remote node and bind our local
    * connection to the specified port
    */
  def connectAndDisconnect(peer: Peer): Future[Assertion] = {

    val probe = TestProbe()
    val peerMessageReceiverF =
      for {
        wallet <- BitcoinSWalletTest.createDLCWallet2Accounts(MockNodeApi,
                                                              MockChainQueryApi,
                                                              bip39PasswordOpt)
      } yield PeerMessageReceiver.preConnection(peer, DLCMessageHandler(wallet))

    val clientActorF: Future[TestActorRef[P2PClientActor]] =
      peerMessageReceiverF.map { peerMsgRecv =>
        TestActorRef(P2PClient.props(peer, peerMsgRecv), probe.ref)
      }
    val p2pClientF: Future[P2PClient] = clientActorF.map {
      client: TestActorRef[P2PClientActor] =>
        P2PClient(client, peer)
    }

    val isConnectedF = for {
      p2pClient <- p2pClientF
      _ = p2pClient.actor ! ConnectCommand
      isConnected <- TestAsyncUtil.retryUntilSatisfiedF(p2pClient.isConnected,
                                                        interval = 1.second,
                                                        maxTries = 100)
    } yield isConnected

    isConnectedF.flatMap { _ =>
      val isDisconnectedF = for {
        p2pClient <- p2pClientF
        _ = p2pClient.actor ! Tcp.Abort
        isDisconnected <-
          TestAsyncUtil.retryUntilSatisfiedF(p2pClient.isDisconnected,
                                             interval = 1.second,
                                             maxTries = 100)
      } yield isDisconnected

      isDisconnectedF.map { _ =>
        succeed
      }
    }
  }

}
