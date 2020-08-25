package org.bitcoins.chain.blockchain

import akka.actor.ActorSystem
import org.bitcoins.core.api.chain.db.BlockHeaderDb
import org.bitcoins.testkit.chain.fixture.ChainFixture
import org.bitcoins.testkit.chain.{BlockHeaderHelper, ChainUnitTest}
import org.scalatest.FutureOutcome

import scala.collection.mutable

class BlockchainTest extends ChainUnitTest {
  override type FixtureParam = ChainFixture

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withChainFixture(test)

  implicit override val system: ActorSystem = ActorSystem("BlockchainTest")

  behavior of "Blockchain"

  it must "connect a new header to the current tip of a blockchain" inFixtured {
    case ChainFixture.Empty =>
      val blockchain = Blockchain.fromHeaders(
        headers = Vector(ChainUnitTest.genesisHeaderDb)
      )

      val newHeader =
        BlockHeaderHelper.buildNextHeader(ChainUnitTest.genesisHeaderDb)

      val connectTip =
        Blockchain.connectTip(header = newHeader.blockHeader, blockchain)

      connectTip match {
        case ConnectTipResult.ExtendChain(_, newChain) =>
          assert(newHeader == newChain.tip)

        case _ @(_: ConnectTipResult.Reorg | _: ConnectTipResult.BadTip) =>
          assert(false)
      }
  }

  it must "reconstruct a blockchain given a child header correctly" inFixtured {
    case ChainFixture.Empty =>
      val accum = new mutable.ArrayBuffer[BlockHeaderDb](5)
      accum.+=(ChainUnitTest.genesisHeaderDb)
      //generate 4 headers
      0.until(4).foreach { _ =>
        val newHeader = BlockHeaderHelper.buildNextHeader(accum.last)
        accum.+=(newHeader)
      }

      //now given the last header, and the other headers we should reconstruct the blockchain
      val headers = accum.dropRight(1).toVector
      val tip = accum.last

      val reconstructed = Blockchain.reconstructFromHeaders(childHeader = tip,
                                                            ancestors = headers)

      assert(reconstructed.length == 1)
      val chain = reconstructed.head
      assert(chain.toVector.length == 5)
      assert(chain.tip == accum.last)
      assert(chain.last == ChainUnitTest.genesisHeaderDb)
      assert(chain.toVector == accum.reverse.toVector)
  }

  it must "fail to reconstruct a blockchain if we do not have validly connected headers" inFixtured {
    case ChainFixture.Empty =>
      val missingHeader =
        BlockHeaderHelper.buildNextHeader(ChainUnitTest.genesisHeaderDb)

      val thirdHeader = BlockHeaderHelper.buildNextHeader(missingHeader)

      val reconstructed =
        Blockchain.reconstructFromHeaders(thirdHeader,
                                          Vector(ChainUnitTest.genesisHeaderDb))

      assert(reconstructed.isEmpty)
  }
}
