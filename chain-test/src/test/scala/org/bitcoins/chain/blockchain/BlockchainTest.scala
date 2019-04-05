package org.bitcoins.chain.blockchain

import org.bitcoins.chain.models.BlockHeaderDAO
import org.bitcoins.chain.util.ChainUnitTest
import org.bitcoins.testkit.chain.BlockHeaderHelper

class BlockchainTest extends ChainUnitTest {

  behavior of "Blockchain"

  it must "connect a new header to the current tip of a blockchain" in withBlockHeaderDAO {
    bhDAO: BlockHeaderDAO =>
      val blockchain = Blockchain.fromHeaders(
        headers = Vector(genesisHeaderDb),
        blockHeaderDAO = bhDAO
      )

      val newHeader = BlockHeaderHelper.buildNextHeader(genesisHeaderDb)

      val connectTipF = blockchain.connectTip(newHeader.blockHeader)

      connectTipF.map {
        case BlockchainUpdate.Successful(_, connectedHeader) =>
          assert(newHeader == connectedHeader)

        case fail: BlockchainUpdate.Failed =>
          assert(false)
      }
  }
}
