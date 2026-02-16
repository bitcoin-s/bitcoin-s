package org.bitcoins.chain.pow

import org.bitcoins.chain.blockchain.Blockchain
import org.bitcoins.core.protocol.blockchain.{
  MainNetChainParams,
  RegTestNetChainParams,
  TestNetChainParams
}
import org.bitcoins.testkit.util.BitcoinSAsyncTest
import org.bitcoins.testkitcore.chain.ChainTestUtil

import scala.concurrent.Future

class BitcoinPowTest extends BitcoinSAsyncTest {

  behavior of "BitcoinPow"

  it must "NOT calculate a POW change when one is not needed" in {
    val blockchain = Blockchain.fromHeaders(
      Vector(ChainTestUtil.ValidPOWChange.blockHeaderDb566494)
    )
    val header2 = ChainTestUtil.ValidPOWChange.blockHeaderDb566495

    val nextWork =
      Pow.getNetworkWorkRequired(
        newPotentialTip = header2.blockHeader,
        blockchain = blockchain,
        chainParams = RegTestNetChainParams
      )

    assert(nextWork == blockchain.tip.nBits)
  }

  it must "calculate a pow change as per the bitcoin network" in {
    val firstBlockDb = ChainTestUtil.ValidPOWChange.blockHeaderDb564480
    val currentTipDb = ChainTestUtil.ValidPOWChange.blockHeaderDb566495
    val expectedNextWork =
      ChainTestUtil.ValidPOWChange.blockHeader566496.nBits
    val calculatedWork =
      Pow.calculateNextWorkRequired(
        currentTipDb,
        firstBlockDb,
        MainNetChainParams
      )

    assert(calculatedWork == expectedNextWork)
  }

  it must "getBlockProof correctly for the testnet genesis block" in {
    Future {
      val header = TestNetChainParams.genesisBlock.blockHeader
      val proof = Pow.getBlockProof(header)

      assert(proof == BigInt(4295032833L))
    }
  }

  it must "getBlockProof correctly for the mainnet genesis block" in {
    Future {
      val header = MainNetChainParams.genesisBlock.blockHeader
      val proof = Pow.getBlockProof(header)

      assert(proof == BigInt(4295032833L))
    }
  }

  it must "handle negative timespan when currentTip time is less than firstBlock time" in {
    Future {
      // Create a scenario where currentTip.time < firstBlock.time
      // This can happen on testnet due to the 20-minute difficulty reset rule
      val firstBlockDb = ChainTestUtil.ValidPOWChange.blockHeaderDb564480
      
      // Create a currentTip with a time that is 2000 seconds BEFORE firstBlock
      val currentTipDb = firstBlockDb.copy(
        time = org.bitcoins.core.number.UInt32(
          Math.max(0, firstBlockDb.time.toLong - 2000)
        ),
        height = firstBlockDb.height + 100
      )
      
      // This should not throw an exception even though timespan is negative
      val calculatedWork = Pow.calculateNextWorkRequired(
        currentTipDb,
        firstBlockDb,
        TestNetChainParams
      )
      
      // The calculated work should be valid (clamped to minimum timespan)
      // When timespan is negative or too small, it gets clamped to timespanSeconds / 4
      assert(calculatedWork != org.bitcoins.core.number.UInt32.zero)
    }
  }
}
