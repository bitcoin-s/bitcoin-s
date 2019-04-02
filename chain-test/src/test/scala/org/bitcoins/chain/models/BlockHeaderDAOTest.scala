package org.bitcoins.chain.models

import org.bitcoins.chain.db.ChainDbManagement
import org.bitcoins.chain.util.ChainUnitTest
import org.bitcoins.core.crypto.{DoubleSha256Digest, ECPrivateKey}
import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.testkit.chain.ChainTestUtil

import scala.concurrent.{Await, ExecutionContext, Future}

/**
  * Created by chris on 9/8/16.
  */
class BlockHeaderDAOTest extends ChainUnitTest {

  val blockHeaderDAO = BlockHeaderDAO(chainParams =
                                        ChainTestUtil.regTestChainParams,
                                      dbConfig = dbConfig)
  before {
    //Awaits need to be used to make sure this is fully executed before the next test case starts
    //TODO: Figure out a way to make this asynchronous
    Await.result(ChainDbManagement.createHeaderTable(dbConfig), timeout)
    Await.result(blockHeaderDAO.create(genesisHeader), timeout)
  }

  behavior of "BlockHeaderDAO"

  it should "insert and read the genesis block header back" in {
    val readF = blockHeaderDAO.read(genesisHeader.hashBE)

    val assert1 = readF.map { readHeader =>
      assert(readHeader.get.blockHeader.hashBE == genesisHeader.hashBE)
    }
    val read1F = blockHeaderDAO.getAtHeight(0)

    val assert2 = {
      read1F.map { headersAtHeight0 =>
        assert(headersAtHeight0 == List(genesisHeader))
      }
    }

    assert1.flatMap(_ => assert2.map(_ => succeed))

  }

  it must "delete a block header in the database" in {

    val blockHeader = buildHeader(genesisHeader)

    val createdF = blockHeaderDAO.create(blockHeader)
    //delete the header in the db
    val deletedF = {
      createdF.flatMap { _ =>
        blockHeaderDAO.delete(blockHeader)
      }
    }

    deletedF.flatMap { _ =>
      blockHeaderDAO
        .read(blockHeader.blockHeader.hashBE)
        .map(opt => assert(opt.isEmpty))
    }

  }

  it must "retrieve the chain tip saved in the database" in {

    val blockHeader = buildHeader(genesisHeader)

    val createdF = blockHeaderDAO.create(blockHeader)

    val chainTip1F = createdF.flatMap { _ =>
      blockHeaderDAO.chainTips
    }

    val assert1F = chainTip1F.map { tips =>
      assert(tips.length == 1)
      assert(tips.head.blockHeader.hash == blockHeader.blockHeader.hash)
    }

    val blockHeader2 = buildHeader(blockHeader)

    //insert another header and make sure that is the new last header
    assert1F.flatMap { _ =>
      val created2F = blockHeaderDAO.create(blockHeader2)
      val chainTip2F = created2F.flatMap(_ => blockHeaderDAO.chainTips)

      chainTip2F.map { tips =>
        assert(tips.length == 1)
        assert(tips.head.blockHeader.hash == blockHeader2.blockHeader.hash)
      }
    }

  }

  it must "return the genesis block when retrieving block headers from an empty database" in {
    val chainTipsF = blockHeaderDAO.chainTips
    chainTipsF.map { tips =>
      assert(tips.headOption == Some(genesisHeader))
    }
  }

  it must "retrieve a block header by height" in {
    val blockHeader = buildHeader(genesisHeader)

    val createdF = blockHeaderDAO.create(blockHeader)

    val getAtHeightF: Future[Vector[BlockHeaderDb]] = {
      createdF.flatMap { _ =>
        blockHeaderDAO.getAtHeight(1)
      }
    }

    val assert1F = getAtHeightF.map {
      case headers =>
        assert(headers.head == blockHeader)
        assert(headers.head.height == 1)
    }

    //create one at height 2
    val blockHeader2 = buildHeader(blockHeader)

    val created2F = blockHeaderDAO.create(blockHeader2)

    val getAtHeight2F: Future[Vector[BlockHeaderDb]] = {
      created2F.flatMap(_ => blockHeaderDAO.getAtHeight(2))
    }

    val assert2F = getAtHeight2F.map { headers =>
      assert(headers.head == blockHeader2)
    }

    assert1F.flatMap(_ => assert2F.map(_ => succeed))
  }

  it must "find the height of the longest chain" in {
    val blockHeader = buildHeader(genesisHeader)
    val createdF = blockHeaderDAO.create(blockHeader)

    val maxHeightF = createdF.flatMap(_ => blockHeaderDAO.maxHeight)

    val blockHeader2 = buildHeader(blockHeader)

    val created2F = maxHeightF.flatMap(_ => blockHeaderDAO.create(blockHeader2))

    val maxHeight2F = created2F.flatMap(_ => blockHeaderDAO.maxHeight)

    maxHeightF.flatMap { h1 =>
      maxHeight2F.map { h2 =>
        assert(h1 == 1)
        assert(h2 == 2)

      }
    }

  }

  it must "find the height of two headers that are competing to be the longest chain" in {
    val blockHeader = buildHeader(genesisHeader)
    val createdF = blockHeaderDAO.create(blockHeader)

    val blockHeader1 = buildHeader(genesisHeader)
    val created2F = createdF.flatMap(_ => blockHeaderDAO.create(blockHeader1))

    //now make sure they are both at height 1
    val getHeightF = created2F.flatMap(_ => blockHeaderDAO.getAtHeight(1))

    getHeightF.map {
      case headers =>
        assert(headers == Seq(blockHeader, blockHeader1))
    }
  }

  after {
    //Awaits need to be used to make sure this is fully executed before the next test case starts
    //TODO: Figure out a way to make this asynchronous
    Await.result(ChainDbManagement.dropHeaderTable(dbConfig), timeout)
  }

  private def buildHeader(prevHeader: BlockHeaderDb): BlockHeaderDb = {
    val prevHash = prevHeader.blockHeader.previousBlockHash
    val blockHeader = {
      BlockHeader(
        version = Int32.one,
        previousBlockHash = prevHash,
        //get random 32 bytes
        merkleRootHash =
          DoubleSha256Digest.fromBytes(ECPrivateKey.freshPrivateKey.bytes),
        time = UInt32.one,
        nBits = UInt32.one,
        nonce = UInt32.one
      )
    }

    BlockHeaderDbHelper.fromBlockHeader(prevHeader.height + 1, blockHeader)
  }

}
