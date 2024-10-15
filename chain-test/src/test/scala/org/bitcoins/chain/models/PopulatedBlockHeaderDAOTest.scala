package org.bitcoins.chain.models

import org.bitcoins.chain.blockchain.Blockchain
import org.bitcoins.chain.pow.Pow
import org.bitcoins.core.protocol.blockchain.MainNetChainParams
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.testkit.chain.{ChainDbUnitTest, ChainUnitTest}
import org.scalatest.{Assertion, FutureOutcome}

import scala.concurrent.Future

class PopulatedBlockHeaderDAOTest extends ChainDbUnitTest {
  override type FixtureParam = BlockHeaderDAO

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withPopulatedBlockHeaderDAO(test)

  it must "GetNextWorkRequired correctly" in {
    case blockHeaderDAO: BlockHeaderDAO =>
      val iterations = 4200
      // We must start after the first POW change to avoid looking for a block we don't have
      val iterator =
        (ChainUnitTest.FIRST_POW_CHANGE + 1)
          .until(ChainUnitTest.FIRST_POW_CHANGE + 1 + iterations)
          .toVector
      val assertionFs: Future[Assertion] = FutureUtil
        .batchExecute(
          elements = iterator,
          f = batchCheckHeaderPOW(_: Vector[Int], blockHeaderDAO),
          init = succeed,
          batchSize = 1000
        )

      assertionFs
  }

  /** Helper method to check headers proof of work in batches */
  private def batchCheckHeaderPOW(
      iterator: Vector[Int],
      blockHeaderDAO: BlockHeaderDAO
  ): Future[Assertion] = {
    val nestedAssertions: Vector[Future[Assertion]] = {
      iterator.map { height =>
        val blockF = blockHeaderDAO.getAtHeight(height + 1).map(_.head)
        val blockchainOptF: Future[Option[Blockchain]] =
          blockF.flatMap(b => blockHeaderDAO.getBlockchainFrom(b))

        blockchainOptF.map {
          case Some(blockchain) =>
            val chain = Blockchain.fromHeaders(blockchain.tail.toVector)
            val nextTip = blockchain.tip
            val nextNBits =
              Pow.getNetworkWorkRequired(nextTip.blockHeader,
                                         chain,
                                         MainNetChainParams)
            assert(nextNBits == nextTip.nBits)
          case None =>
            fail(s"Chain not found best on header at height=$height")
        }
      }
    }
    Future
      .sequence(nestedAssertions)
      .map(_ => succeed)
  }
}
