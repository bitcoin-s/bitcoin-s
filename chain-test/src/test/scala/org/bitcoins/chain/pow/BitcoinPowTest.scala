package org.bitcoins.chain.pow

import akka.actor.ActorSystem
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.db.ChainDbConfig
import org.bitcoins.chain.models.BlockHeaderDAO
import org.bitcoins.chain.util.ChainUnitTest
import org.bitcoins.core.protocol.blockchain.MainNetChainParams
import org.bitcoins.db.NetworkDb
import org.bitcoins.testkit.chain.ChainTestUtil
import org.scalatest.FutureOutcome

class BitcoinPowTest extends ChainUnitTest {

  override type FixtureParam = ChainFixture

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withChainFixture(test)

  override implicit val system: ActorSystem = ActorSystem("BitcoinPowTest")

  behavior of "BitcoinPow"

  it must "NOT calculate a POW change when one is not needed" inFixtured {
    case ChainFixture.Empty =>
      val dbConfig = ChainDbConfig.UnitTestDbConfig(NetworkDb.MainNetDbConfig)
      val appConfig = ChainAppConfig(dbConfig)
      val blockHeaderDAO = BlockHeaderDAO(appConfig)
      val header1 = ChainTestUtil.ValidPOWChange.blockHeaderDb566494
      val header2 = ChainTestUtil.ValidPOWChange.blockHeaderDb566495

      val nextWorkF = Pow.getNetworkWorkRequired(header1,
                                                 header2.blockHeader,
                                                 blockHeaderDAO)

      nextWorkF.map(nextWork => assert(nextWork == header1.nBits))
  }

  it must "calculate a pow change as per the bitcoin network" inFixtured {
    case ChainFixture.Empty =>
      val firstBlockDb = ChainTestUtil.ValidPOWChange.blockHeaderDb564480
      val currentTipDb = ChainTestUtil.ValidPOWChange.blockHeaderDb566495
      val expectedNextWork =
        ChainTestUtil.ValidPOWChange.blockHeader566496.nBits
      val calculatedWorkF =
        Pow.calculateNextWorkRequired(currentTipDb,
                                      firstBlockDb,
                                      MainNetChainParams)

      calculatedWorkF.map(calculatedWork =>
        assert(calculatedWork == expectedNextWork))
  }

  it must "calculate a GetNextWorkRequired correctly" taggedAs FixtureTag.PopulatedBlockHeaderDAO inFixtured {
    case ChainFixture.PopulatedBlockHeaderDAO(blockHeaderDAO) => succeed
  }
}
