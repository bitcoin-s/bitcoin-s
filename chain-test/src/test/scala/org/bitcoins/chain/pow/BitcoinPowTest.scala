package org.bitcoins.chain.pow

import akka.actor.ActorSystem
import org.bitcoins.chain.blockchain.Blockchain
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.BlockHeaderDAO
import org.bitcoins.core.protocol.blockchain.{
  MainNetChainParams,
  TestNetChainParams
}
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.testkit.chain.fixture.{ChainFixture, ChainFixtureTag}
import org.bitcoins.testkit.chain.{
  ChainDbUnitTest,
  ChainTestUtil,
  ChainUnitTest
}
import org.scalatest.{Assertion, FutureOutcome}

import scala.concurrent.Future

class BitcoinPowTest extends ChainDbUnitTest {

  override type FixtureParam = ChainFixture

  // we're working with mainnet data
  implicit override lazy val cachedChainConf: ChainAppConfig = mainnetAppConfig

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withChainFixture(test)

  implicit override val system: ActorSystem = ActorSystem("BitcoinPowTest")

  behavior of "BitcoinPow"

  it must "NOT calculate a POW change when one is not needed" inFixtured {
    case ChainFixture.Empty =>
      val blockchain = Blockchain.fromHeaders(
        Vector(ChainTestUtil.ValidPOWChange.blockHeaderDb566494))
      val header2 = ChainTestUtil.ValidPOWChange.blockHeaderDb566495

      val nextWork =
        Pow.getNetworkWorkRequired(newPotentialTip = header2.blockHeader,
                                   blockchain = blockchain)

      assert(nextWork == blockchain.tip.nBits)
  }

  it must "calculate a pow change as per the bitcoin network" inFixtured {
    case ChainFixture.Empty =>
      val firstBlockDb = ChainTestUtil.ValidPOWChange.blockHeaderDb564480
      val currentTipDb = ChainTestUtil.ValidPOWChange.blockHeaderDb566495
      val expectedNextWork =
        ChainTestUtil.ValidPOWChange.blockHeader566496.nBits
      val calculatedWork =
        Pow.calculateNextWorkRequired(currentTipDb,
                                      firstBlockDb,
                                      MainNetChainParams)

      assert(calculatedWork == expectedNextWork)
  }

  it must "GetNextWorkRequired correctly" taggedAs ChainFixtureTag.PopulatedBlockHeaderDAO inFixtured {
    case ChainFixture.PopulatedBlockHeaderDAO(blockHeaderDAO) =>
      val iterations = 4200
      // We must start after the first POW change to avoid looking for a block we don't have
      val iterator =
        (ChainUnitTest.FIRST_POW_CHANGE + 1)
          .until(ChainUnitTest.FIRST_POW_CHANGE + 1 + iterations)
          .toVector
      val assertionFs: Future[Assertion] = FutureUtil
        .batchExecute(elements = iterator,
                      f = batchCheckHeaderPOW(_: Vector[Int], blockHeaderDAO),
                      init = succeed,
                      batchSize = 1000)

      assertionFs
  }

  it must "getBlockProof correctly for the testnet genesis block" inFixtured {
    case ChainFixture.Empty =>
      Future {
        val header = TestNetChainParams.genesisBlock.blockHeader
        val proof = Pow.getBlockProof(header)

        assert(proof == BigInt(4295032833L))
      }
  }

  it must "getBlockProof correctly for the mainnet genesis block" inFixtured {
    case ChainFixture.Empty =>
      Future {
        val header = MainNetChainParams.genesisBlock.blockHeader
        val proof = Pow.getBlockProof(header)

        assert(proof == BigInt(4295032833L))
      }
  }

  /** Helper method to check headers proof of work in batches */
  private def batchCheckHeaderPOW(
      iterator: Vector[Int],
      blockHeaderDAO: BlockHeaderDAO): Future[Assertion] = {
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
              Pow.getNetworkWorkRequired(nextTip.blockHeader, chain)
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
