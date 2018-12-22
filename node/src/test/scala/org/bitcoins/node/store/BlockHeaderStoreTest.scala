package org.bitcoins.node.store

import org.bitcoins.core.gen.BlockchainElementsGenerator
import org.scalatest.{BeforeAndAfter, FlatSpec, MustMatchers}

/**
  * Created by chris on 9/5/16.
  */
class BlockHeaderStoreTest
    extends FlatSpec
    with MustMatchers
    with BeforeAndAfter {
  val testFile = new java.io.File("src/test/resources/block_header.dat")

  before {
    testFile.createNewFile()
  }

  "BlockHeaderStore" must "write and then read a block header from the database" in {
    val blockHeader = BlockchainElementsGenerator.blockHeader.sample.get
    BlockHeaderStore.append(Seq(blockHeader), testFile)
    val headersFromFile = BlockHeaderStore.read(testFile)

    headersFromFile must be(Seq(blockHeader))
  }

  it must "write one blockheader to the file, then append another header to the file, then read them both" in {
    val blockHeader1 = BlockchainElementsGenerator.blockHeader.sample.get
    val blockHeader2 = BlockchainElementsGenerator.blockHeader.sample.get
    BlockHeaderStore.append(Seq(blockHeader1), testFile)
    val headersFromFile1 = BlockHeaderStore.read(testFile)
    headersFromFile1 must be(Seq(blockHeader1))

    BlockHeaderStore.append(Seq(blockHeader2), testFile)
    val headersFromFile2 = BlockHeaderStore.read(testFile)
    headersFromFile2 must be(Seq(blockHeader1, blockHeader2))
  }

  after {
    testFile.delete()
  }

}
