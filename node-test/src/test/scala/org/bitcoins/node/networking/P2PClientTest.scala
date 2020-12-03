package org.bitcoins.node.networking

import akka.io.Tcp
import akka.testkit.{TestActorRef, TestProbe}
import org.bitcoins.core.config.TestNet3
import org.bitcoins.core.number.{Int32, UInt32, UInt64}
import org.bitcoins.core.p2p._
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.crypto.{CryptoUtil, DoubleSha256Digest}
import org.bitcoins.node.NodeCallbacks
import org.bitcoins.node.models.Peer
import org.bitcoins.node.networking.peer.PeerMessageReceiver
import org.bitcoins.testkit.async.TestAsyncUtil
import org.bitcoins.testkit.node.{CachedBitcoinSAppConfig, NodeTestUtil}
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.testkit.util.BitcoindRpcTest
import org.scalatest._
import scodec.bits._

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

class P2PClientTest extends BitcoindRpcTest with CachedBitcoinSAppConfig {

  lazy val bitcoindRpcF =
    BitcoindRpcTestUtil.startedBitcoindRpcClient(clientAccum = clientAccum)

  lazy val bitcoindPeerF = bitcoindRpcF.map { bitcoind =>
    NodeTestUtil.getBitcoindPeer(bitcoind)
  }

  lazy val bitcoindRpc2F =
    BitcoindRpcTestUtil.startedBitcoindRpcClient(clientAccum = clientAccum)

  lazy val bitcoindPeer2F = bitcoindRpcF.map { bitcoind =>
    NodeTestUtil.getBitcoindPeer(bitcoind)
  }
  behavior of "parseIndividualMessages"

  it must "block header message that is not aligned with a tcp frame" in {
    implicit val nodeConf = config.nodeConf

    val headersMsg = HeadersMessage(
      CompactSizeUInt(UInt64(2), 1),
      Vector(
        BlockHeader(
          Int32(315017594),
          DoubleSha256Digest(
            "177e777f078d2deeaa3ad4b82e78a00ad2f4738c5217f7a36d9cf3bd11e41817"),
          DoubleSha256Digest(
            "1dcaebebd620823bb344bd18a18276de508910d66b4e3cbb3426a14eced66224"),
          UInt32(2845833462L),
          UInt32(2626024374L),
          UInt32(2637850613L)
        ),
        BlockHeader(
          Int32(1694049746),
          DoubleSha256Digest(
            "07b6d61809476830bc7ef862a983a7222997df3f639e0d2aa5902a5a48018430"),
          DoubleSha256Digest(
            "68c65f803b70b72563e86ac3e8e20ad11fbfa2eac3f9fddf4bc624d03a14f084"),
          UInt32(202993555),
          UInt32(4046619225L),
          UInt32(1231236881)
        )
      )
    )
    val networkMsg = NetworkMessage(np, headersMsg)
    //split the network msg at a random index to simulate a tcp frame not being aligned
    val randomIndex = scala.util.Random.nextInt().abs % networkMsg.bytes.size
    val (firstHalf, secondHalf) = networkMsg.bytes.splitAt(randomIndex)
    val (firstHalfParseHeaders, remainingBytes) =
      P2PClient.parseIndividualMessages(firstHalf)
    firstHalfParseHeaders must be(empty)

    val (secondHalfParsedHeaders, _) =
      P2PClient.parseIndividualMessages(remainingBytes ++ secondHalf)
    val parsedNetworkMsg = secondHalfParsedHeaders.head
    val parsedHeadersMsg = parsedNetworkMsg.payload.asInstanceOf[HeadersMessage]
    parsedNetworkMsg.header must be(networkMsg.header)
    parsedHeadersMsg.headers.head must be(headersMsg.headers.head)
    parsedHeadersMsg.headers(1) must be(parsedHeadersMsg.headers(1))

  }

  it must "return the entire byte array if a message is not aligned to a byte frame" in {
    implicit val nodeConf = config.nodeConf
    val versionMessage =
      VersionMessage(TestNet3.dnsSeeds(0), np)
    val networkMsg = NetworkMessage(np, versionMessage)
    //remove last byte so the message is not aligned
    val bytes = networkMsg.bytes.slice(0, networkMsg.bytes.size - 1)
    val (_, unAlignedBytes) = P2PClient.parseIndividualMessages(bytes)

    unAlignedBytes must be(bytes)
  }

  // we had a bug where we didn't consume the right number of bytes
  // when parsing a merkle block message, thereby screwing up
  // the parsing of the remainder
  it must "parse a byte vector with three messages in it" in {
    implicit val nodeConf = config.nodeConf
    val bytes =
      hex"fabfb5da6d65726b6c65626c6f636b0097000000b4b6e45d00000020387191f7d488b849b4080fdf105c71269fc841a2f0f2944fc5dc785c830c716e37f36373098aae06a668cc74e388caf50ecdcb5504ce936490b4b72940e08859548c305dffff7f20010000000200000002ecd1c722709bfc241f8b94fc64034dcba2c95409dc4cd1d7b864e1128a04e5b044133327b04ff8ac576e7748a4dae4111f0c765dacbfe0c5a9fddbeb8f60d5af0105fabfb5da747800000000000000000000cc0100004413332702000000065b7f0f3eec398047e921037815aa41709b6243a1897f1423194b7558399ae0300000000017160014008dc9d88d1797305f3fbd30d2b36d6bde984a09feffffffe9145055d671fd705a09f028033da614b619205b9926fe5ebe45e15ae8b3231e0100000017160014d74cfac04bb0e6838c35f1f4a0a60d13655be2fbfeffffff797f8ff9c10fa618b6254343a648be995410e82c03fd8accb0de2271a3fb1abd00000000171600143ee832c09db48eca28a64a358ed7a01dbe52d31bfeffffffc794dba971b9479dfcbc662a3aacd641553bdb2418b15c0221c5dfd4471a7a70000000001716001452c13ba0314f7718c234ed6adfea6422ce03a545feffffffb7c3bf1762b15f3b0e0eaa5beb46fe96a9e2829a7413fd900b9b7e0d192ab64800000000171600143ee832c09db48eca28a64a358ed7a01dbe52d31bfeffffffb6ced6cb8dfc2f7f5b37561938ead3bc5ca4036e2b45d9738cc086a10eed4e010100000017160014aebb17e245fe8c98a75f0b6717fcadca30e491e2feffffff02002a7515000000001976a9148374ff8beb55ea2945039881ca26071b5749fafe88ac485620000000000017a91405d36a2b0bdedf3fc58bed6f9e4026f8934a2716876b050000fabfb5da686561646572730000000000010000001406e05800"
    val (messages, leftover) = P2PClient.parseIndividualMessages(bytes)
    assert(messages.length == 3)

    val commandNames = messages.map(_.header.commandName)
    assert(commandNames == Vector("merkleblock", "tx", "headers"))

    assert(leftover.isEmpty)
  }

  it must "parse an unknown command" in {
    val header: NetworkHeader =
      NetworkHeader(TestNet3,
                    "madeup",
                    UInt32.zero,
                    CryptoUtil.doubleSHA256(ByteVector.empty).bytes.take(4))

    val (messages, leftover) = P2PClient.parseIndividualMessages(header.bytes)
    assert(messages.isEmpty)
    assert(leftover.isEmpty)
  }

  behavior of "P2PClient"

  override def beforeAll(): Unit = {
    implicit val chainConf = config.chainConf
    chainConf.migrate()
    ()
  }

  override def afterAll(): Unit = {
    implicit val chainConf = config.chainConf
    val shutdownConfigF = for {
      _ <- chainConf.dropTable("flyway_schema_history")
      _ <- chainConf.dropAll()
    } yield {
      super[CachedBitcoinSAppConfig].afterAll()
    }

    shutdownConfigF.onComplete { _ =>
      super[BitcoindRpcTest].afterAll()
    }

  }

  it must "establish a tcp connection with a bitcoin node" in {
    bitcoindPeerF.flatMap { remote =>
      connectAndDisconnect(remote)
    }
  }

  it must "connect to two nodes" in {
    val try1 =
      bitcoindPeerF.flatMap(remote => connectAndDisconnect(remote))

    val try2 = bitcoindPeer2F.flatMap(remote => connectAndDisconnect(remote))

    try1.flatMap { _ =>
      try2
    }
  }

  /**
    * Helper method to connect to the
    * remote node and bind our local
    * connection to the specified port
    */
  def connectAndDisconnect(peer: Peer): Future[Assertion] = {
    implicit val chainConf = config.chainConf
    implicit val nodeConf = config.nodeConf

    val probe = TestProbe()
    val remote = peer.socket
    val peerMessageReceiverF =
      PeerMessageReceiver.preConnection(peer, NodeCallbacks.empty, None)

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
      _ = p2pClient.actor ! Tcp.Connect(remote)
      isConnected <- TestAsyncUtil.retryUntilSatisfiedF(p2pClient.isConnected)
    } yield isConnected

    isConnectedF.flatMap { _ =>
      val isDisconnectedF = for {
        p2pClient <- p2pClientF
        _ = p2pClient.actor ! Tcp.Abort
        isDisconnected <-
          TestAsyncUtil.retryUntilSatisfiedF(p2pClient.isDisconnected,
                                             interval = 1.seconds)
      } yield isDisconnected

      isDisconnectedF.map { _ =>
        succeed
      }
    }
  }

}
