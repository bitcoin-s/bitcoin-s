package org.bitcoins.node

import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.rpc.util.RpcUtil
import org.bitcoins.testkit.node.NodeUnitTest
import org.bitcoins.testkit.node.fixture.SpvNodeConnectedWithBitcoind
import org.scalatest.FutureOutcome

import scala.concurrent.Future
import org.bitcoins.testkit.node.NodeTestUtil

class SpvNodeTest extends NodeUnitTest {

  override type FixtureParam = SpvNodeConnectedWithBitcoind

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withSpvNodeConnectedToBitcoind(test)

  behavior of "SpvNode"

  it must "receive notification that a block occurred on the p2p network" in {
    spvNodeConnectedWithBitcoind: SpvNodeConnectedWithBitcoind =>
      val spvNode = spvNodeConnectedWithBitcoind.spvNode
      val bitcoind = spvNodeConnectedWithBitcoind.bitcoind

      assert(spvNode.isConnected)

      assert(spvNode.isInitialized)

      val hashF: Future[DoubleSha256DigestBE] = {
        bitcoind.generate(1).map(_.head)
      }

      //check we have that hash inside of our chain project!
      val spvSyncF = for {
        _ <- hashF
        sync <- spvNode.sync()
      } yield sync

      spvSyncF.flatMap { _ =>
        NodeTestUtil
          .awaitSync(spvNode, bitcoind)
          .map(_ => succeed)
      }

  }
}
