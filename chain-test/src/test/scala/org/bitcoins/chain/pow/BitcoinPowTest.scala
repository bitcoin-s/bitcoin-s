package org.bitcoins.chain.pow

import org.bitcoins.chain.models.BlockHeaderDAO
import org.bitcoins.chain.util.ChainUnitTest
import org.bitcoins.core.protocol.blockchain.MainNetChainParams
import org.bitcoins.db.UnitTestDbConfig
import org.bitcoins.testkit.chain.ChainTestUtil

class BitcoinPowTest extends ChainUnitTest {

  behavior of "BitcoinPow"

  it must "NOT calculate a POW change when one is not needed" in {
    val chainParams = MainNetChainParams
    val blockHeaderDAO = BlockHeaderDAO(chainParams, UnitTestDbConfig)
    val header1 = ChainTestUtil.ValidPOWChange.blockHeaderDb566494
    val header2 = ChainTestUtil.ValidPOWChange.blockHeaderDb566495

    val nextWorkF = BitcoinPow.getNetworkWorkRequired(header1,
                                                      header2.blockHeader,
                                                      blockHeaderDAO,
                                                      chainParams)

    nextWorkF.map(nextWork => assert(nextWork == header2.nBits))
  }

  it must "calculate a pow change as per the bitcoin network" in {
    val firstBlockDb = ChainTestUtil.ValidPOWChange.blockHeaderDb564480
    val currentTipDb = ChainTestUtil.ValidPOWChange.blockHeaderDb566495
    val expectedNextWork = ChainTestUtil.ValidPOWChange.blockHeader566496.nBits
    val calculatedWorkF =
      BitcoinPow.calculateNextWorkRequired(currentTipDb,
                                           firstBlockDb,
                                           MainNetChainParams)

    calculatedWorkF.map(calculatedWork =>
      assert(calculatedWork == expectedNextWork))
  }

  it must "calculate a GetNextWorkRequired correctly"
}
