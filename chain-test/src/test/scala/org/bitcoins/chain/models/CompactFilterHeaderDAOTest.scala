package org.bitcoins.chain.models

import org.bitcoins.core.api.chain.db.{BlockHeaderDb, CompactFilterHeaderDb}
import org.bitcoins.testkit.chain.{
  BlockHeaderHelper,
  ChainDbUnitTest,
  ChainTestUtil
}
import org.bitcoins.testkit.core.gen.CryptoGenerators
import org.scalatest.FutureOutcome

class CompactFilterHeaderDAOTest extends ChainDbUnitTest {

  override type FixtureParam = CompactFilterHeaderDAO

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withCompactFilterHeaderDAO(test)

  behavior of "CompactFilterHeaderDAO"

  it must "get the best filter header with a table with zero rows in it" in {
    filterHeaderDAO =>
      filterHeaderDAO.getBestFilterHeader.map { opt =>
        assert(opt.isEmpty)
      }
  }

  it must "create and read a filter header from the database" in {
    filterHeaderDAO =>
      val blockHeaderDAO = BlockHeaderDAO()
      val blockHeaderDb =
        BlockHeaderHelper.buildNextHeader(ChainTestUtil.regTestGenesisHeaderDb)
      val blockHeaderDbF = blockHeaderDAO.create(blockHeaderDb)
      val filterHeaderDb1F = for {
        blockHeaderDb <- blockHeaderDbF
      } yield {
        randomFilterHeader(blockHeaderDb)
      }

      val createdF = filterHeaderDb1F.flatMap(filterHeaderDAO.create)

      for {
        headerDb <- createdF
        original <- filterHeaderDb1F
        fromDbOpt <- filterHeaderDAO.read(headerDb.hashBE)
      } yield {
        assert(fromDbOpt.isDefined)
        assert(original == fromDbOpt.get)
      }
  }

  it must "find filters between height" in { filterHeaderDAO =>
    val blockHeaderDAO = BlockHeaderDAO()
    val blockHeaderDb =
      BlockHeaderHelper.buildNextHeader(ChainTestUtil.regTestGenesisHeaderDb)
    val blockHeaderDbF = blockHeaderDAO.create(blockHeaderDb)
    val filterHeaderDb1F = for {
      blockHeaderDb <- blockHeaderDbF
    } yield {
      randomFilterHeader(blockHeaderDb)
    }

    val createdF = filterHeaderDb1F.flatMap(filterHeaderDAO.create)

    for {
      headerDb <- createdF
      fromDbVec <-
        filterHeaderDAO.getBetweenHeights(headerDb.height, headerDb.height)
    } yield {
      assert(fromDbVec.length == 1)
      assert(fromDbVec.head == headerDb)
    }
  }

  it must "get the best filter header that has a block header associated with it" in {
    filterHeaderDAO =>
      val blockHeaderDAO = BlockHeaderDAO()
      val blockHeaderDb = {
        BlockHeaderHelper.buildNextHeader(ChainTestUtil.regTestGenesisHeaderDb)
      }
      val blockHeaderDbF = blockHeaderDAO.create(blockHeaderDb)
      val filterHeaderDb1F = for {
        blockHeaderDb <- blockHeaderDbF
      } yield {
        randomFilterHeader(blockHeaderDb)
      }

      val createdF = filterHeaderDb1F.flatMap(filterHeaderDAO.create)
      for {
        blockHeader <- blockHeaderDbF
        _ <- createdF
        headers = Vector(blockHeader)
        found <- filterHeaderDAO.getBestFilterHeaderForHeaders(headers)
        empty <- filterHeaderDAO.getBestFilterHeaderForHeaders(Vector.empty)
      } yield {
        assert(found.nonEmpty)
        assert(empty.isEmpty)
      }
  }

  it must "fail to find a filter header if the block header is not in the db" in {
    filterHeaderDAO =>
      val blockHeaderDb = {
        BlockHeaderHelper.buildNextHeader(ChainTestUtil.regTestGenesisHeaderDb)
      }
      val headers = Vector(blockHeaderDb)

      for {
        resultOpt <- filterHeaderDAO.getBestFilterHeaderForHeaders(headers)
      } yield {
        assert(resultOpt.isEmpty)
      }
  }

  it must "find the filter header with the heaviest work" in {
    filterHeaderDAO =>
      val blockHeaderDAO = BlockHeaderDAO()
      val blockHeaderDbLightWork = {
        BlockHeaderHelper.buildNextHeader(ChainTestUtil.regTestGenesisHeaderDb)
      }

      val blockHeaderDbHeavyWork = {
        blockHeaderDbLightWork.copy(
          chainWork = blockHeaderDbLightWork.chainWork + 1,
          hashBE = CryptoGenerators.doubleSha256Digest.sample.get.flip)
      }
      val headers = Vector(blockHeaderDbLightWork, blockHeaderDbHeavyWork)
      val blockHeaderDbF = blockHeaderDAO.createAll(headers)
      val filterHeaderDbLightWork = {
        randomFilterHeader(blockHeaderDbLightWork)
      }

      val filterHeaderDbHeavyWork = {
        randomFilterHeader(blockHeaderDbHeavyWork)
      }

      val filterHeaders =
        Vector(filterHeaderDbLightWork, filterHeaderDbHeavyWork)

      val createdF = for {
        _ <- blockHeaderDbF
        created <- filterHeaderDAO.createAll(filterHeaders)
      } yield created

      for {
        _ <- createdF
        found <- filterHeaderDAO.getBestFilterHeader
      } yield {
        assert(found.nonEmpty)
        assert(found.get == filterHeaderDbHeavyWork)
      }
  }

  private def randomFilterHeader(
      blockHeader: BlockHeaderDb): CompactFilterHeaderDb = {
    CompactFilterHeaderDb(
      CryptoGenerators.doubleSha256Digest.sample.get.flip,
      filterHashBE = CryptoGenerators.doubleSha256Digest.sample.get.flip,
      previousFilterHeaderBE =
        CryptoGenerators.doubleSha256Digest.sample.get.flip,
      blockHashBE = blockHeader.hashBE,
      blockHeader.height
    )
  }
}
