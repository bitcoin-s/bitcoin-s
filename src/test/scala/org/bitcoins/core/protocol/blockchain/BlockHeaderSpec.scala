package org.bitcoins.core.protocol.blockchain

import org.bitcoins.core.gen.BlockchainElementsGenerator
import org.scalacheck.{ Prop, Properties }

/**
 * Created by tom on 7/6/16.
 */
class BlockHeaderSpec extends Properties("BlockHeaderSpec") {
  property("serialization symmetry") =
    Prop.forAll(BlockchainElementsGenerator.blockHeader) { header =>
      BlockHeader(header.hex) == header
    }
}
