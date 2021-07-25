package org.bitcoins.dlc.node

import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.dlc.models.DLCState
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.dlc.node.DLCClient.Connect
import org.bitcoins.dlc.node.peer.Peer
import org.bitcoins.rpc.util.RpcUtil
import org.bitcoins.testkit.async.TestAsyncUtil
import org.bitcoins.testkit.wallet.BitcoinSDualWalletTest
import org.bitcoins.testkit.wallet.DLCWalletUtil._
import org.bitcoins.testkit.wallet.FundWalletUtil.FundedDLCWallet
import org.scalatest.FutureOutcome

import java.net.InetSocketAddress
import scala.concurrent.duration.DurationInt

class DLCNegotiationTest extends BitcoinSDualWalletTest {
  type FixtureParam = (FundedDLCWallet, FundedDLCWallet)

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withDualFundedDLCWallets(test)
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

      val _ = system.actorOf(DLCServer.props(walletA, bindAddress))
      val client = system.actorOf(DLCClient.props(walletB))

      client ! Connect(Peer(connectAddress))

      for {
        // verify we have no DLCs
        preDLCsA <- walletA.listDLCs()
        preDLCsB <- walletB.listDLCs()
        _ = assert(preDLCsA.isEmpty)
        _ = assert(preDLCsB.isEmpty)

        offer <- walletA.createDLCOffer(sampleContractInfo,
                                        half,
                                        Some(SatoshisPerVirtualByte.one),
                                        UInt32.zero,
                                        UInt32.one)
        accept <- walletB.acceptDLCOffer(offer)

        // Send accept message to begin p2p
        _ = client ! accept.toMessage

        _ <- TestAsyncUtil.awaitConditionF(
          () =>
            for {
              dlcsA <- walletA.listDLCs()
              dlcsB <- walletB.listDLCs()
            } yield {
              println(dlcsA)
              println(dlcsB)
              println("------------")
              dlcsA.head.state == DLCState.Signed &&
              dlcsB.head.state == DLCState.Broadcasted
            },
          interval = 1.second,
          maxTries = 5
        )
      } yield succeed
  }
}
