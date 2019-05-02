package org.bitcoins.chain.blockchain

import akka.actor.ActorSystem
import org.bitcoins.chain.models.BlockHeaderDAO
import org.bitcoins.chain.util.ChainUnitTest
import org.bitcoins.testkit.chain.BlockHeaderHelper
import org.scalatest.FutureOutcome

class BlockchainTest extends ChainUnitTest {

  override type FixtureParam = BlockHeaderDAO

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withBlockHeaderDAO(test)

  override implicit val system: ActorSystem = ActorSystem("BlockchainTest")

  behavior of "Blockchain"

  it must "connect a new header to the current tip of a blockchain" in {
    bhDAO: BlockHeaderDAO =>
      val blockchain = Blockchain.fromHeaders(
        headers = Vector(genesisHeaderDb)
      )

      val newHeader = BlockHeaderHelper.buildNextHeader(genesisHeaderDb)

      val connectTipF = Blockchain.connectTip(header = newHeader.blockHeader,blockHeaderDAO = bhDAO)

      connectTipF.map {
        case BlockchainUpdate.Successful(_, connectedHeader) =>
          assert(newHeader == connectedHeader)

        case fail: BlockchainUpdate.Failed =>
          assert(false)
      }
  }
}
