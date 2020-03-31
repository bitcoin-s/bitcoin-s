package org.bitcoins.node.networking.peer

import _root_.org.scalatest.compatible.Assertion
import org.bitcoins.core.protocol.blockchain.{Block, MerkleBlock}
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.node.NodeCallbacks
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.core.gen.{
  BlockchainElementsGenerator,
  TransactionGenerators
}
import org.bitcoins.testkit.util.BitcoinSAsyncTest
import org.scalacheck.Gen

import scala.util.{Failure, Success, Try}

class MerkleBuffersTest extends BitcoinSAsyncTest {

  implicit private val config: NodeAppConfig =
    BitcoinSTestAppConfig.getSpvTestConfig().nodeConf

  behavior of "MerkleBuffers"

  it must "match a merkle block with its corresponding transactions" in {

    val txsAndBlockGen: Gen[(Seq[Transaction], Seq[Transaction], Block)] = for {
      txs <- TransactionGenerators.nonEmptySmallTransactions
      otherTxs <- TransactionGenerators.nonEmptySmallTransactions
      block <- BlockchainElementsGenerator.block(txs)
    } yield (txs, otherTxs, block)

    forAll(txsAndBlockGen) {

      case (txs, otherTxs, block) =>
        var receivedExpectedTXs: Option[Try[Assertion]] = None
        var callbackCount: Int = 0
        val callback: DataMessageHandler.OnMerkleBlockReceived = {
          (_, merkleTxs) =>
            receivedExpectedTXs = Some(
              Try(
                assert(txs == merkleTxs,
                       "Received TXs in callback was not the ones we put in")))
            callbackCount = callbackCount + 1
            FutureUtil.unit
        }
        val callbacks = NodeCallbacks(onMerkleBlockReceived = Seq(callback))

        val merkle = MerkleBlock(block, txs.map(_.txId))
        val _ = MerkleBuffers.putMerkle(merkle)

        txs.map { tx =>
          MerkleBuffers
            .putTx(tx, callbacks)
            .map(matches =>
              assert(
                matches,
                s"TX ${tx.txIdBE} did not match any merkle block in MerkleBuffers"))
        }

        otherTxs.map { tx =>
          MerkleBuffers
            .putTx(tx, callbacks)
            .map(matches =>
              assert(
                !matches,
                s"Unrelated TX ${tx.txIdBE} did match merkle block in MerkleBuffers"))
        }

        assert(callbackCount != 0,
               "Callback was not called after processing all TXs!")

        assert(callbackCount == 1,
               s"Callback was called multiple times: $callbackCount")

        receivedExpectedTXs match {
          case None                     => fail("Callback was never called")
          case Some(Success(assertion)) => assertion
          case Some(Failure(exc))       => fail(exc)
        }

    }

  }
}
