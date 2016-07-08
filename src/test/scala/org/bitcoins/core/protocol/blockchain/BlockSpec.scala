package org.bitcoins.core.protocol.blockchain

import org.bitcoins.core.gen.BlockchainElementsGenerator
import org.scalacheck.{Prop, Properties}

/**
  * Created by tom on 7/6/16.
  */
class BlockSpec extends Properties("BlockSpec") {
  property("Serialization symmetry") =
  Prop.forAll(BlockchainElementsGenerator.block) { block =>
    Block(block.blockHeader, block.txCount, block.transactions).hex == block.hex
  }

}
