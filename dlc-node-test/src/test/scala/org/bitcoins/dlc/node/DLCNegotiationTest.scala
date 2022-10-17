package org.bitcoins.dlc.node

import akka.actor.ActorRef
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BigSizeUInt
import org.bitcoins.core.protocol.dlc.models.DLCState
import org.bitcoins.core.protocol.tlv.{LnMessage, SendOfferTLV}
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.dlc.node.peer.Peer
import org.bitcoins.rpc.util.RpcUtil
import org.bitcoins.testkit.async.TestAsyncUtil
import org.bitcoins.testkit.wallet.BitcoinSDualWalletTest
import org.bitcoins.testkit.wallet.DLCWalletUtil._
import org.bitcoins.testkit.wallet.FundWalletUtil.FundedDLCWallet
import org.scalatest.FutureOutcome
import scodec.bits.ByteVector

import java.net.InetSocketAddress
import scala.concurrent.{Future, Promise}
import scala.concurrent.duration.DurationInt

class DLCNegotiationTest extends BitcoinSDualWalletTest {
  type FixtureParam = (FundedDLCWallet, FundedDLCWallet)

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withDualFundedDLCWallets(test)
  }

  private val handleWriteFn: (BigSizeUInt, ByteVector) => Future[Unit] = {
    case (_: BigSizeUInt, _: ByteVector) =>
      Future.unit
  }

  private val handleWriteErrorFn: (
      BigSizeUInt,
      ByteVector,
      Throwable) => Future[Unit] = {
    case (_: BigSizeUInt, _: ByteVector, _: Throwable) =>
      Future.unit
  }

  it must "setup a DLC" in {
    fundedDLCWallets: (FundedDLCWallet, FundedDLCWallet) =>
      val walletA = fundedDLCWallets._1.wallet
      val walletB = fundedDLCWallets._2.wallet
      val port = RpcUtil.randomPort
      val bindAddress =
        new InetSocketAddress("0.0.0.0", port)
      val connectAddress =
        InetSocketAddress.createUnresolved("127.0.0.1", port)

      val handlerP = Promise[ActorRef]()

      for {
        _ <- DLCServer.bind(dlcWalletApi = walletA,
                            bindAddress = bindAddress,
                            targets = Vector(),
                            torParams = None,
                            handleWrite = handleWriteFn,
                            handleWriteError = handleWriteErrorFn)
        _ <- DLCClient.connect(Peer(connectAddress, socks5ProxyParams = None),
                               walletB,
                               Some(handlerP),
                               handleWrite = handleWriteFn,
                               handleWriteError = handleWriteErrorFn)

        handler <- handlerP.future

        // verify we have no DLCs
        preDLCsA <- walletA.listDLCs()
        preDLCsB <- walletB.listDLCs()
        _ = assert(preDLCsA.isEmpty)
        _ = assert(preDLCsB.isEmpty)

        offer <- walletB.createDLCOffer(sampleContractInfo,
                                        half,
                                        Some(SatoshisPerVirtualByte.one),
                                        UInt32.zero,
                                        UInt32.one,
                                        None,
                                        None,
                                        None)
        accept <- walletA.acceptDLCOffer(offer, None, None, None)

        // Send accept message to begin p2p
        _ = handler ! DLCDataHandler.Received(accept.toMessage)

        _ <- TestAsyncUtil.awaitConditionF(
          () =>
            for {
              dlcsA <- walletA.listDLCs()
              dlcsB <- walletB.listDLCs()
            } yield {
              dlcsA.head.state == DLCState.Broadcasted &&
              dlcsB.head.state == DLCState.Signed
            },
          interval = 1.second,
          maxTries = 15
        )
      } yield succeed
  }

  it must "receive an offer over" in {
    fundedDLCWallets: (FundedDLCWallet, FundedDLCWallet) =>
      val walletA = fundedDLCWallets._1.wallet
      val walletB = fundedDLCWallets._2.wallet
      val port = RpcUtil.randomPort
      val bindAddress =
        new InetSocketAddress("0.0.0.0", port)
      val connectAddress =
        InetSocketAddress.createUnresolved("127.0.0.1", port)

      val handlerP = Promise[ActorRef]()
      val okP = Promise[ByteVector]()
      val errorP = Promise[ByteVector]()

      for {
        _ <- DLCServer.bind(walletA,
                            bindAddress,
                            Vector(),
                            None,
                            handleWrite = handleWriteFn,
                            handleWriteError = handleWriteErrorFn)
        _ <- DLCClient.connect(
          Peer(connectAddress, socks5ProxyParams = None),
          walletB,
          Some(handlerP),
          handleWrite = { (_, tlvId) =>
            okP.success(tlvId)
            Future.unit
          },
          handleWriteError = { (_, tlvId, ex) =>
            errorP.success(tlvId)
            Future.failed(ex)
          }
        )

        handler <- handlerP.future

        preA <- walletA.listIncomingDLCOffers()
        preB <- walletA.listIncomingDLCOffers()
        _ = assert(preA.isEmpty)
        _ = assert(preB.isEmpty)

        offer <- walletB.createDLCOffer(sampleContractInfo,
                                        half,
                                        Some(SatoshisPerVirtualByte.one),
                                        UInt32.zero,
                                        UInt32.one,
                                        None,
                                        None,
                                        None)
        tlv = SendOfferTLV(peer = "peer", message = "msg", offer = offer.toTLV)

        _ = assert(!okP.isCompleted)
        _ = assert(!errorP.isCompleted)
        _ = handler ! DLCDataHandler.Send(LnMessage(tlv))
        ok <- okP.future
        _ = assert(ok == tlv.offer.tempContractId.bytes)
        _ = assert(!errorP.isCompleted)

        _ <- TestAsyncUtil.awaitConditionF { () =>
          walletA.listIncomingDLCOffers().map(_.nonEmpty)
        }
        postA <- walletA.listIncomingDLCOffers()
        postB <- walletB.listIncomingDLCOffers()
      } yield {
        assert(postA.nonEmpty)
        assert(postB.isEmpty)
        assert(postA.head.peer.get == "peer")
        assert(postA.head.message.get == "msg")
        assert(postA.head.offerTLV == offer.toTLV)
      }
  }
}
