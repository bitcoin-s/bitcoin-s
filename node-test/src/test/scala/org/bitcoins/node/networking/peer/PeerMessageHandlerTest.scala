package org.bitcoins.node.networking.peer

import akka.testkit.TestKit
import akka.util.Timeout
import org.bitcoins.testkit.async.TestAsyncUtil
import org.bitcoins.testkit.node.NodeUnitTest
import org.scalatest.FutureOutcome

import scala.concurrent.duration.DurationInt

/**
  * Created by chris on 7/1/16.
  */
class PeerMessageHandlerTest extends NodeUnitTest {
  override type FixtureParam = Unit
  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    test(())
  }

  implicit private val akkaTimeout = Timeout(timeout)

  behavior of "PeerHandler"

  it must "be able to fully initialize a PeerMessageReceiver" in { _ =>
    val peerHandlerF =
      bitcoindPeerF.flatMap(p => NodeUnitTest.buildPeerHandler(p))
    val peerMsgSenderF = peerHandlerF.map(_.peerMsgSender)
    val p2pClientF = peerHandlerF.map(_.p2pClient)

    val _ =
      bitcoindPeerF.flatMap(p => peerHandlerF.map(_.peerMsgSender.connect()))

    val isConnectedF = TestAsyncUtil.retryUntilSatisfiedF(
      () => p2pClientF.flatMap(_.isConnected),
      duration = 500.millis
    )

    val isInitF = isConnectedF.flatMap { _ =>
      TestAsyncUtil.retryUntilSatisfiedF(() =>
        p2pClientF.flatMap(_.isInitialized()))
    }

    val disconnectF = isInitF.flatMap { _ =>
      peerMsgSenderF.map(_.disconnect())
    }

    val isDisconnectedF = disconnectF.flatMap { _ =>
      TestAsyncUtil.retryUntilSatisfiedF(() =>
        p2pClientF.flatMap(_.isDisconnected()))

    }

    isDisconnectedF.map(_ => succeed)
  }

  /*
    "PeerMessageHandler" must "be able to send a GetHeadersMessage then receive a list of headers back" in {

      val hashStart = DoubleSha256Digest.empty
      //this is the hash of block 2, so this test will send two blocks
      val hashStop = DoubleSha256Digest(
        BitcoinSUtil.flipEndianness(
          "000000006c02c8ea6e4ff69651f7fcde348fb9d557a06e6957b65552002a7820"))
      val getHeadersMessage =
        GetHeadersMessage(Constants.version, List(hashStart), hashStop)

      val (peerMsgSender, testProbe) = buildPeerMessageSender()
      val socket = peerSocketAddress
      val peerHandler = PeerHandler(dbConfig = NodeTestUtil.dbConfig,
                                    peerMsgSender = peerMsgSender,
                                    socket = socket)

      val connected = Await.result(peerHandler.connect(), timeout)

      val _ = peerHandler.getHeaders(getHeadersMsg = getHeadersMessage)

      val headersMsg = expectMsgType[HeadersMessage](timeout)

      headersMsg.commandName must be(NetworkPayload.headersCommandName)

      val firstHeader = headersMsg.headers.head

      firstHeader.hash.hex must be(
        BitcoinSUtil.flipEndianness(
          "00000000b873e79784647a6c82962c70d228557d24a747ea4d1b8bbe878e1206"))

      val secondHeader = headersMsg.headers(1)
      secondHeader.hash.hex must be(
        BitcoinSUtil.flipEndianness(
          "000000006c02c8ea6e4ff69651f7fcde348fb9d557a06e6957b65552002a7820"))

      peerHandler.close()

    }

        it must "send a getblocks message and receive a list of blocks back" in {
          val hashStart = DoubleSha256Digest(
            "0000000000000000000000000000000000000000000000000000000000000000")
          //this is the hash of block 2, so this test will send two blocks
          val hashStop = DoubleSha256Digest(
            BitcoinSUtil.flipEndianness(
              "000000006c02c8ea6e4ff69651f7fcde348fb9d557a06e6957b65552002a7820"))

          val getBlocksMsg =
            GetBlocksMessage(Constants.version, Seq(hashStart), hashStop)

          val peerRequest = buildPeerRequest(getBlocksMsg)

          val (peerMsgHandler, probe) = peerMsgHandlerRef
          probe.send(peerMsgHandler, peerRequest)

          val invMsg = probe.expectMsgType[InventoryMessage](5.seconds)

          invMsg.inventoryCount must be(CompactSizeUInt(UInt64.one, 1))
          invMsg.inventories.head.hash.hex must be(
            BitcoinSUtil.flipEndianness(
              "00000000b873e79784647a6c82962c70d228557d24a747ea4d1b8bbe878e1206"))
          invMsg.inventories.head.typeIdentifier must be(MsgBlock)
          peerMsgHandler ! Tcp.Close
          probe.expectMsg(Tcp.Closed)
        }

        it must "request a full block from another node" in {
          //first block on testnet
          //https://tbtc.blockr.io/block/info/1
          val blockHash = DoubleSha256Digest(
            BitcoinSUtil.flipEndianness(
              "00000000b873e79784647a6c82962c70d228557d24a747ea4d1b8bbe878e1206"))
          val getDataMessage = GetDataMessage(Inventory(MsgBlock, blockHash))
          val peerRequest = buildPeerRequest(getDataMessage)
          val (peerMsgHandler, probe) = peerMsgHandlerRef
          probe.send(peerMsgHandler, peerRequest)

          val blockMsg = probe.expectMsgType[BlockMessage](5.seconds)
          logger.debug("BlockMsg: " + blockMsg)
          blockMsg.block.blockHeader.hash must be(blockHash)

          blockMsg.block.transactions.length must be(1)
          blockMsg.block.transactions.head.txId must be
          (DoubleSha256Digest(
            BitcoinSUtil.flipEndianness(
              "f0315ffc38709d70ad5647e22048358dd3745f3ce3874223c80a7c92fab0c8ba")))
          peerMsgHandler ! Tcp.Close
          probe.expectMsg(Tcp.Closed)

        }

        it must "request a transaction from another node" in {
          //this tx is the coinbase tx in the first block on testnet
          //https://tbtc.blockr.io/tx/info/f0315ffc38709d70ad5647e22048358dd3745f3ce3874223c80a7c92fab0c8ba
          val txId = DoubleSha256Digest(
            BitcoinSUtil.flipEndianness(
              "a4dd00d23de4f0f96963e16b72afea547bc9ad1d0c1dda5653110eddd83fe0e2"))
          val getDataMessage = GetDataMessage(Inventory(MsgTx, txId))
          val peerRequest = buildPeerRequest(getDataMessage)
          val (peerMsgHandler, probe) = peerMsgHandlerRef
          probe.send(peerMsgHandler, peerRequest)
          //we cannot request an arbitrary tx from a node,
          //therefore the node responds with a [[NotFoundMessage]]
          probe.expectMsgType[NotFoundMessage](5.seconds)

          peerMsgHandler ! Tcp.Close
          probe.expectMsg(Tcp.Closed)
        }

        it must "send a GetAddressMessage and then receive an AddressMessage back" in {
          val (peerMsgHandler, probe) = peerMsgHandlerRef
          val peerRequest = buildPeerRequest(GetAddrMessage)
          probe.send(peerMsgHandler, peerRequest)
          val addrMsg = probe.expectMsgType[AddrMessage](15.seconds)
          peerMsgHandler ! Tcp.Close
          probe.expectMsg(Tcp.Closed)
        }

        it must "send a PingMessage to our peer and receive a PongMessage back" in {
          val (peerMsgHandler, probe) = peerMsgHandlerRef
          val nonce = UInt64(scala.util.Random.nextLong.abs)

          val peerRequest = buildPeerRequest(PingMessage(nonce))

          system.scheduler.schedule(2.seconds,
                                    30.seconds,
                                    peerMsgHandler,
                                    peerRequest)(global, probe.ref)
          val pongMessage = probe.expectMsgType[PongMessage](8.seconds)

          pongMessage.nonce must be(nonce)

          peerMsgHandler ! Tcp.Close
          probe.expectMsg(Tcp.Closed)
        }*/

  override def afterAll = {
    startedBitcoindF.flatMap(_.stop())
    TestKit.shutdownActorSystem(system)
  }
}
