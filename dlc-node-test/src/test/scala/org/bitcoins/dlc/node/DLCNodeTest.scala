package org.bitcoins.dlc.node

import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.dlc.models.DLCState
import org.bitcoins.core.util.NetworkUtil
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.testkit.async.TestAsyncUtil
import org.bitcoins.testkit.dlc.BitcoinSDLCNodeTest
import org.bitcoins.testkit.wallet.DLCWalletUtil._
import org.scalatest.FutureOutcome

import java.net.InetSocketAddress
import scala.concurrent.{Future, Promise}
import scala.concurrent.duration.DurationInt

class DLCNodeTest extends BitcoinSDLCNodeTest {
  type FixtureParam = (DLCNode, DLCNode)

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    witTwoFundedDLCNodes(test)
  }

  it must "check a connection" in { nodes: (DLCNode, DLCNode) =>
    val nodeA = nodes._1
    val nodeB = nodes._2

    val configA = nodeA.config

    val errorP = Promise[String]()
    val failure =
      DLCNodeCallbacks.onPeerConnectionFailed(new OnPeerConnectionFailed {
        override def apply(param: InetSocketAddress): Future[Unit] = {
          errorP.success("err")
          Future.unit
        }
      })
    configA.addCallbacks(failure)

    val okP = Promise[String]()
    val established = DLCNodeCallbacks.onPeerConnectionEstablished(
      new OnPeerConnectionEstablished {
        override def apply(param: InetSocketAddress): Future[Unit] = {
          okP.success("ok")
          Future.unit
        }
      })
    configA.addCallbacks(established)

    val initiatedP = Promise[String]()
    val init =
      DLCNodeCallbacks.onPeerConnectionInitiated(new OnPeerConnectionInitiated {
        override def apply(param: InetSocketAddress): Future[Unit] = {
          initiatedP.success("init")
          Future.unit
        }
      })
    configA.addCallbacks(init)

    for {
      (addrB, _) <- nodeB.serverBindF
      _ = assert(!initiatedP.isCompleted)
      _ = assert(!errorP.isCompleted)
      _ = assert(!okP.isCompleted)
      // don't wait for the future completion here, we want to test OnPeerConnectionInitiated
      _ = nodeA.checkPeerConnection(addrB)
      initiated <- initiatedP.future
      _ = assert(initiated == "init")
      ok <- okP.future
      _ = assert(ok == "ok")
      _ = assert(!errorP.isCompleted)
      invalidAddr = InetSocketAddress.createUnresolved(addrB.getHostString,
                                                       NetworkUtil.randomPort())
      _ <- recoverToSucceededIf[Exception](
        nodeA.checkPeerConnection(invalidAddr))
      error <- errorP.future
    } yield {
      assert(error == "err")
    }
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
                                      UInt32.one,
                                      None,
                                      None,
                                      None)

      _ <- nodeB.acceptDLCOffer(addrA, offer.toMessage, None, None)

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
