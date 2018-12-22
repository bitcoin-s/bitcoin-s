package org.bitcoins.node.store

import org.bitcoins.core.gen.BlockchainElementsGenerator
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.scalacheck.{Gen, Prop, Properties}

/**
  * Created by chris on 9/5/16.
  */
class BlockHeaderStoreSpec extends Properties("BlockHeaderStoreSpec") {

  property("serialization symmetry to file") =
    Prop.forAll(Gen.listOf(BlockchainElementsGenerator.blockHeader)) {
      case headers: Seq[BlockHeader] =>
        val file =
          new java.io.File("src/test/resources/block_header_spec_1.dat")
        BlockHeaderStore.append(headers, file)
        val headersFromFile = BlockHeaderStore.read(file)
        val result = headersFromFile == headers
        file.delete()
        result
    }

  property("read the last stored blockheader stored in a file") =
    Prop.forAll(Gen.listOf(BlockchainElementsGenerator.blockHeader)) {
      case headers: Seq[BlockHeader] =>
        val file =
          new java.io.File("src/test/resources/block_header_spec_2.dat")
        BlockHeaderStore.append(headers, file)
        val lastHeader = BlockHeaderStore.lastHeader(file)
        val expectedLastHeader =
          if (headers.isEmpty) None else Some(headers.last)
        val result = lastHeader == expectedLastHeader
        file.delete()
        result
    }
}
