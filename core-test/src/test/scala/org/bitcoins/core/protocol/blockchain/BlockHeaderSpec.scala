package org.bitcoins.core.protocol.blockchain

import org.bitcoins.testkitcore.gen.BlockchainElementsGenerator
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

/** Created by tom on 7/6/16.
  */
class BlockHeaderSpec extends BitcoinSUnitTest {

  it must "serialization symmetry" in {
    forAll(BlockchainElementsGenerator.blockHeader) { header =>
      assert(BlockHeader(header.hex) == header)
    }
  }
}
