package org.bitcoins.chain.models

import akka.actor.ActorSystem
import org.bitcoins.testkit.chain.{BlockHeaderHelper, ChainUnitTest}
import org.scalatest.FutureOutcome

import scala.concurrent.Future

/**
  * Created by chris on 9/8/16.
  */
class BlockHeaderDAOTest extends ChainUnitTest {

  override type FixtureParam = BlockHeaderDAO

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withBlockHeaderDAO(test)

  implicit override val system: ActorSystem = ActorSystem("BlockHeaderDAOTest")

  behavior of "BlockHeaderDAO"

  private val genesisHeaderDb = ChainUnitTest.genesisHeaderDb
  it should "insert and read the genesis block header back" in {
    blockHeaderDAO: BlockHeaderDAO =>
      val readF = blockHeaderDAO.read(genesisHeaderDb.hashBE)

      val assert1 = readF.map { readHeader =>
        assert(readHeader.get.blockHeader.hashBE == genesisHeaderDb.hashBE)
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
    blockHeaderDAO: BlockHeaderDAO =>
      val blockHeader = BlockHeaderHelper.buildNextHeader(genesisHeaderDb)

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
    blockHeaderDAO: BlockHeaderDAO =>
      val blockHeader = BlockHeaderHelper.buildNextHeader(genesisHeaderDb)

      val createdF = blockHeaderDAO.create(blockHeader)

      val chainTip1F = createdF.flatMap { _ =>
        blockHeaderDAO.chainTips
      }

      val assert1F = chainTip1F.map { tips =>
        assert(tips.length == 1)
        assert(tips.head.blockHeader.hash == blockHeader.blockHeader.hash)
      }

      val blockHeader2 = BlockHeaderHelper.buildNextHeader(blockHeader)

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
    blockHeaderDAO: BlockHeaderDAO =>
      val chainTipsF = blockHeaderDAO.chainTips
      chainTipsF.map { tips =>
        assert(tips.headOption == Some(genesisHeaderDb))
      }
  }

  it must "retrieve a block header by height" in {
    blockHeaderDAO: BlockHeaderDAO =>
      val blockHeader = BlockHeaderHelper.buildNextHeader(genesisHeaderDb)

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
      val blockHeader2 = BlockHeaderHelper.buildNextHeader(blockHeader)

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
    blockHeaderDAO: BlockHeaderDAO =>
      val blockHeader = BlockHeaderHelper.buildNextHeader(genesisHeaderDb)
      val createdF = blockHeaderDAO.create(blockHeader)

      val maxHeightF = createdF.flatMap(_ => blockHeaderDAO.maxHeight)

      val blockHeader2 = BlockHeaderHelper.buildNextHeader(blockHeader)

      val created2F =
        maxHeightF.flatMap(_ => blockHeaderDAO.create(blockHeader2))

      val maxHeight2F = created2F.flatMap(_ => blockHeaderDAO.maxHeight)

      maxHeightF.flatMap { h1 =>
        maxHeight2F.map { h2 =>
          assert(h1 == 1)
          assert(h2 == 2)

        }
      }

  }

  it must "find the height of two headers that are competing to be the longest chain" in {
    blockHeaderDAO: BlockHeaderDAO =>
      val blockHeader = BlockHeaderHelper.buildNextHeader(genesisHeaderDb)
      val createdF = blockHeaderDAO.create(blockHeader)

      val blockHeader1 = BlockHeaderHelper.buildNextHeader(genesisHeaderDb)
      val created2F = createdF.flatMap(_ => blockHeaderDAO.create(blockHeader1))

      //now make sure they are both at height 1
      val getHeightF = created2F.flatMap(_ => blockHeaderDAO.getAtHeight(1))

      getHeightF.map {
        case headers =>
          assert(headers == Seq(blockHeader, blockHeader1))
      }
  }

  it must "find a header with height 1" in { blockHeaderDAO: BlockHeaderDAO =>
    val blockHeader = BlockHeaderHelper.buildNextHeader(genesisHeaderDb)
    val createdF = blockHeaderDAO.create(blockHeader)

    val f: BlockHeaderDb => Boolean = { bh =>
      bh.height == 1
    }

    val foundF = createdF.flatMap(created => blockHeaderDAO.find(f))

    for {
      created <- createdF
      found <- foundF
    } yield assert(found.get == created)
  }
}
