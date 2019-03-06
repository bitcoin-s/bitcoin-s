package org.bitcoins.chain.models

import org.bitcoins.chain.config.ChainDbManagement
import org.bitcoins.core.crypto.{
  DoubleSha256Digest,
  DoubleSha256DigestBE,
  ECPrivateKey
}
import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.db.{DbConfig, UnitTestDbConfig}
import org.bitcoins.node.constant.Constants
import org.bitcoins.node.db.NodeDbManagement
import org.bitcoins.testkit.chain.ChainTestUtil
import org.bitcoins.testkit.core.gen.BlockchainElementsGenerator
import org.scalatest._
import org.slf4j.LoggerFactory

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Failure

/**
  * Created by chris on 9/8/16.
  */
class BlockHeaderDAOTest
    extends AsyncFlatSpec
    with MustMatchers
    with BeforeAndAfter
    with BeforeAndAfterAll {
  implicit val ec: ExecutionContext =
    scala.concurrent.ExecutionContext.Implicits.global
  val timeout = 10.seconds
  val dbConfig: DbConfig = UnitTestDbConfig

  val genesisHeaderDb = ChainTestUtil.regTestHeaderDb
  val chainParams = ChainTestUtil.regTestChainParams

  val logger = LoggerFactory.getLogger(getClass)

  val blockHeaderDAO =
    BlockHeaderDAO(chainParams = chainParams, dbConfig = dbConfig)
  before {
    //Awaits need to be used to make sure this is fully executed before the next test case starts
    //TODO: Figure out a way to make this asynchronous
    val createdTableF = ChainDbManagement.createBlockHeaderTable(dbConfig)
    val insertHeaderF =
      createdTableF.flatMap(_ => blockHeaderDAO.create(genesisHeaderDb))
    Await.result(insertHeaderF, timeout)
  }

  behavior of "BlockHeaderDAO"

  it should "insert and read the genesis block header back" in {
    val readF = blockHeaderDAO.read(genesisHeaderDb.hashBE)

    val assert1 = readF.map { readHeader =>
      assert(readHeader.get.hashBE == genesisHeaderDb.hashBE)
    }
    val read1F = blockHeaderDAO.getAtHeight(0)

    val assert2 = {
      read1F.map { headersAtHeight0 =>
        assert(headersAtHeight0 == List(genesisHeaderDb))
      }
    }

    assert1.flatMap(_ => assert2.map(_ => succeed))

  }

  it must "delete a block header in the database" in {

    val blockHeader = buildHeader(genesisHeaderDb)

    val createdF = blockHeaderDAO.create(blockHeader)

    //delete the header in the db
    val deletedF = {
      createdF.flatMap { _ =>
        blockHeaderDAO.delete(blockHeader)
      }
    }

    deletedF.flatMap { _ =>
      blockHeaderDAO
        .read(blockHeader.hashBE)
        .map(opt => assert(opt.isEmpty))
    }

  }
  it must "retrieve the chain tip saved in the database" in {

    val blockHeaderDb = buildHeader(genesisHeaderDb)
    val blockHeader = blockHeaderDb.blockHeader
    val createdF = blockHeaderDAO.create(blockHeaderDb)

    val chainTip1F = createdF.flatMap { _ =>
      blockHeaderDAO.chainTips
    }

    val assert1F = chainTip1F.map { tips =>
      assert(tips.length == 1)
      assert(tips.head.hashBE == blockHeaderDb.hashBE)
    }

    val blockHeader2 =
      BlockchainElementsGenerator.blockHeader(blockHeader.hash).sample.get
    val blockHeaderDb2 = BlockHeaderDbHelper.fromBlockHeader(2, blockHeader2)
    //insert another header and make sure that is the new last header
    assert1F.flatMap { _ =>
      val created2F = blockHeaderDAO.create(blockHeaderDb2)
      val chainTip2F = created2F.flatMap(_ => blockHeaderDAO.chainTips)

      chainTip2F.map { tips =>
        assert(tips.length == 1)
        assert(tips.head.hashBE == blockHeader2.hashBE)
      }
    }

  }

  it must "return the genesis block when retrieving block headers from an empty database" in {
    val chainTipsF = blockHeaderDAO.chainTips
    chainTipsF.map { tips =>
      assert(tips.headOption == Some(genesisHeaderDb))
    }
  }

  it must "retrieve a block header by height" in {
    val blockHeaderDb = buildHeader(genesisHeaderDb)

    val createdF = blockHeaderDAO.create(blockHeaderDb)

    val getAtHeightF: Future[Vector[BlockHeaderDb]] = {
      createdF.flatMap { _ =>
        blockHeaderDAO.getAtHeight(1)
      }
    }

    val assert1F = getAtHeightF.map {
      case headers =>
        assert(headers.head == blockHeaderDb)
        assert(headers.head.height == 1)
    }

    //create one at height 2
    val blockHeader2 =
      BlockchainElementsGenerator
        .blockHeader(blockHeaderDb.blockHeader.hash)
        .sample
        .get
    val blockHeaderDb2 = BlockHeaderDbHelper.fromBlockHeader(2, blockHeader2)
    val created2F = blockHeaderDAO.create(blockHeaderDb2)

    val getAtHeight2F: Future[Vector[BlockHeaderDb]] = {
      created2F.flatMap(_ => blockHeaderDAO.getAtHeight(2))
    }

    val assert2F = getAtHeight2F.map {
      case headers =>
        assert(headers.head == blockHeaderDb2)
        assert(headers.head.height == 2)
    }

    assert1F.flatMap(_ => assert2F.map(_ => succeed))
  }

  it must "find the height of a block header" in {
    val blockHeader = buildHeader(genesisHeaderDb)
    val createdF = blockHeaderDAO.create(blockHeader)

    val findHeightF = createdF.flatMap { _ =>
      blockHeaderDAO.findByHash(blockHeader.hashBE)
    }

    findHeightF.map { findHeightOpt =>
      assert(findHeightOpt.get == blockHeader)
    }
  }

  it must "not find the height of a header that DNE in the database" in {
    val blockHeader = buildHeader(genesisHeaderDb)

    val foundHashF = blockHeaderDAO.findByHash(blockHeader.hashBE)

    foundHashF.map(f => assert(f == None))
  }

  it must "find the height of the longest chain" in {
    val blockHeader = buildHeader(genesisHeaderDb)
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
    val blockHeader = buildHeader(genesisHeaderDb)
    val createdF = blockHeaderDAO.create(blockHeader)

    val blockHeader1 = buildHeader(genesisHeaderDb)
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
    Await.result(ChainDbManagement.dropBlockHeaderTable(dbConfig), timeout)
  }

  private def buildHeader(prevHeaderDb: BlockHeaderDb): BlockHeaderDb = {
    val header = BlockHeader(
      version = Int32.one,
      previousBlockHash = prevHeaderDb.hashBE.flip,
      //get random 32 bytes
      merkleRootHash =
        DoubleSha256Digest.fromBytes(ECPrivateKey.freshPrivateKey.bytes),
      time = UInt32.one,
      nBits = UInt32.one,
      nonce = UInt32.one
    )

    BlockHeaderDbHelper.fromBlockHeader(
      height = prevHeaderDb.height + 1,
      bh = header
    )
  }
}
