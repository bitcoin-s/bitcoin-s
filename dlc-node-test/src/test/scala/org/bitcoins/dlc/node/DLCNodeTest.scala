package org.bitcoins.dlc.node

import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.dlc.models.DLCState
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.testkit.async.TestAsyncUtil
import org.bitcoins.testkit.dlc.BitcoinSDLCNodeTest
import org.bitcoins.testkit.wallet.DLCWalletUtil._
import org.scalatest.FutureOutcome

import scala.concurrent.duration.DurationInt

class DLCNodeTest extends BitcoinSDLCNodeTest {
  type FixtureParam = (DLCNode, DLCNode)

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    witTwoFundedDLCNodes(test)
  }

  it must "setup a DLC" in { nodes: (DLCNode, DLCNode) =>
    val nodeA = nodes._1
    val nodeB = nodes._2
    val walletA = nodeA.wallet
    val walletB = nodeB.wallet

    for {
      (addrA, _) <- nodeA.serverBindF
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

      _ <- nodeB.acceptDLCOffer(addrA, offer.toMessage)

      _ <- TestAsyncUtil.awaitConditionF(
        () =>
          for {
            dlcsA <- walletA.listDLCs()
            dlcsB <- walletB.listDLCs()
          } yield {
            dlcsA.head.state == DLCState.Signed &&
            dlcsB.head.state == DLCState.Broadcasted
          },
        interval = 1.second,
        maxTries = 15
      )
    } yield succeed
  }
}
