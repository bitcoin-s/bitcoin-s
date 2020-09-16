package org.bitcoins.chain.models

import org.bitcoins.core.api.chain.db.{BlockHeaderDb, CompactFilterDb}
import org.bitcoins.core.gcs.FilterType
import org.bitcoins.crypto.{CryptoUtil, ECPrivateKey}
import org.bitcoins.testkit.chain.{
  BlockHeaderHelper,
  ChainDbUnitTest,
  ChainTestUtil
}
import org.bitcoins.testkit.core.gen.CryptoGenerators
import org.scalatest.FutureOutcome

class CompactFilterDAOTest extends ChainDbUnitTest {

  override type FixtureParam = CompactFilterDAO

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withCompactFilterDAO(test)

  behavior of "CompactFilterDAO"

  it must "retrieve getBestFilter when there are no filters in the db" in {
    compactFilterDAO: CompactFilterDAO =>
      compactFilterDAO.getBestFilter
        .map(opt => assert(opt.isEmpty))
  }

  it must "create and read a filter from the database" in { compactFilterDAO =>
    val blockHeaderDAO = BlockHeaderDAO()
    val filterHeaderDAO = CompactFilterHeaderDAO()
    val blockHeaderDb = ChainTestUtil.regTestGenesisHeaderDb
    val filterHeaderDb = ChainTestUtil.regTestGenesisHeaderCompactFilterHeaderDb
    val original = ChainTestUtil.regTestGenesisHeaderCompactFilterDb

    for {
      _ <- blockHeaderDAO.create(blockHeaderDb)
      _ <- filterHeaderDAO.create(filterHeaderDb)
      _ <- compactFilterDAO.create(original)
      fromDbOpt <- compactFilterDAO.read(original.blockHashBE)
    } yield assert(fromDbOpt.contains(original))
  }

  it must "find filters between heights" in { compactFilterDAO =>
    val blockHeaderDAO = BlockHeaderDAO()
    val blockHeaderDb =
      BlockHeaderHelper.buildNextHeader(ChainTestUtil.regTestGenesisHeaderDb)
    val blockHeaderDbF = blockHeaderDAO.create(blockHeaderDb)
    val filterDb1F = for {
      blockHeaderDb <- blockHeaderDbF
    } yield {
      randomFilter(blockHeaderDb)
    }

    val createdF = filterDb1F.flatMap(compactFilterDAO.create)

    for {
      headerDb <- createdF
      fromDbVec <-
        compactFilterDAO.getBetweenHeights(headerDb.height, headerDb.height)
    } yield {
      assert(fromDbVec.length == 1)
      assert(fromDbVec.head == headerDb)
    }
  }

  it must "get filters between heights when there are no filters" in {
    compactFilterDAO =>
      compactFilterDAO.getBetweenHeights(0, 1).map { result =>
        assert(result.isEmpty)
      }
  }

  it must "get max height when there are no filters" in { compactFilterDAO =>
    compactFilterDAO.maxHeight.map { result =>
      assert(result == 0)
    }
  }

  it must "find the filter with the heaviest work" in { compactFilterDAO =>
    val blockHeaderDAO = BlockHeaderDAO()
    val blockHeaderDbLightWork = {
      BlockHeaderHelper.buildNextHeader(ChainTestUtil.regTestGenesisHeaderDb)
    }

    val blockHeaderDbHeavyWork = {
      blockHeaderDbLightWork.copy(
        chainWork =
          blockHeaderDbLightWork.chainWork + 1,
        hashBE = CryptoGenerators.doubleSha256Digest.sample.get.flip)
    }
    val headers = Vector(blockHeaderDbLightWork, blockHeaderDbHeavyWork)
    val blockHeaderDbF = blockHeaderDAO.createAll(headers)
    val filterDbLightWork = {
      randomFilter(blockHeaderDbLightWork)
    }

    val filterDbHeavyWork = {
      randomFilter(blockHeaderDbHeavyWork)
    }

    val filters =
      Vector(filterDbLightWork, filterDbHeavyWork)

    val createdF = for {
      _ <- blockHeaderDbF
      created <- compactFilterDAO.createAll(filters)
    } yield created

    for {
      _ <- createdF
      found <- compactFilterDAO.getBestFilter
    } yield {
      assert(found.nonEmpty)
      assert(found.get == filterDbHeavyWork)
    }
  }

  private def randomFilter(blockHeader: BlockHeaderDb): CompactFilterDb = {
    val randBytes = ECPrivateKey.freshPrivateKey.bytes
    CompactFilterDb(
      CryptoUtil.doubleSHA256(randBytes).flip,
      filterType = FilterType.Basic,
      bytes = randBytes,
      blockHashBE = blockHeader.hashBE,
      height = blockHeader.height
    )
  }
}
