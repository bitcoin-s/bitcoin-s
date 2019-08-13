package org.bitcoins.chain.pow

import akka.actor.ActorSystem
import org.bitcoins.chain.blockchain.Blockchain
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.BlockHeaderDAO
import org.bitcoins.core.protocol.blockchain.MainNetChainParams
import org.bitcoins.testkit.chain.fixture.{ChainFixture, ChainFixtureTag}
import org.bitcoins.testkit.chain.{ChainTestUtil, ChainUnitTest}
import org.scalatest.FutureOutcome

import scala.concurrent.Future
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig

class BitcoinPowTest extends ChainUnitTest {

  override type FixtureParam = ChainFixture

  implicit override lazy val appConfig: ChainAppConfig = {
    import BitcoinSTestAppConfig.ProjectType
    val memoryDb =
      BitcoinSTestAppConfig.configWithMemoryDb(Some(ProjectType.Chain))
    mainnetAppConfig.withOverrides(memoryDb)
  }

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
      val assertionFs =
        (ChainUnitTest.FIRST_POW_CHANGE + 1 until ChainUnitTest.FIRST_POW_CHANGE + 1 + iterations)
          .map { height =>
            val blockF = blockHeaderDAO.getAtHeight(height).map(_.head)
            val blockchainF =
              blockF.flatMap(b => blockHeaderDAO.getBlockchainFrom(b))
            val nextBlockF = blockHeaderDAO.getAtHeight(height + 1).map(_.head)

            for {
              blockchain <- blockchainF
              nextTip <- nextBlockF
              nextNBits = Pow.getNetworkWorkRequired(nextTip.blockHeader,
                                                     blockchain)
            } yield assert(nextNBits == nextTip.nBits)
          }

      Future.sequence(assertionFs).map(_ => succeed)
  }
}
