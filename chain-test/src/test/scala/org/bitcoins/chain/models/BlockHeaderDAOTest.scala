package org.bitcoins.chain.models

import akka.actor.ActorSystem
import org.bitcoins.chain.blockchain.Blockchain
import org.bitcoins.core.api.chain.db.{BlockHeaderDb, BlockHeaderDbHelper}
import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.core.util.TimeUtil
import org.bitcoins.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import org.bitcoins.testkit.chain.{
  BlockHeaderHelper,
  ChainDbUnitTest,
  ChainTestUtil,
  ChainUnitTest
}
import org.scalatest.FutureOutcome
import scodec.bits._

import scala.concurrent.Future

/**
  * Created by chris on 9/8/16.
  */
class BlockHeaderDAOTest extends ChainDbUnitTest {

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

  it must "fail to find a header closest to a epoch second" in {
    blockHeaderDAO: BlockHeaderDAO =>
      val blockHeader = BlockHeaderHelper.buildNextHeader(genesisHeaderDb)
      val createdF = blockHeaderDAO.create(blockHeader)

      val headerDbF =
        createdF.flatMap(_ => blockHeaderDAO.findClosestToTime(UInt32.zero))

      recoverToSucceededIf[IllegalArgumentException](headerDbF)
  }

  it must "find the closest block to the given epoch second" in {
    blockHeaderDAO: BlockHeaderDAO =>
      val blockHeader = BlockHeaderHelper.buildNextHeader(genesisHeaderDb)
      val createdF = blockHeaderDAO.create(blockHeader)

      val headerDbF = createdF.flatMap(_ =>
        blockHeaderDAO.findClosestToTime(UInt32(TimeUtil.currentEpochSecond)))

      headerDbF.map { headerDb =>
        assert(headerDb == blockHeader)
      }
  }

  it must "find the block at given epoch second" in {
    blockHeaderDAO: BlockHeaderDAO =>
      val blockHeader = BlockHeaderHelper.buildNextHeader(genesisHeaderDb)
      val createdF = blockHeaderDAO.create(blockHeader)

      val headerDbF = createdF.flatMap(_ =>
        blockHeaderDAO.findClosestToTime(blockHeader.time))

      headerDbF.map { headerDb =>
        assert(headerDb == blockHeader)
      }
  }

  it must "find all the headers before to the given epoch second" in {
    blockHeaderDAO: BlockHeaderDAO =>
      val blockHeader = BlockHeaderHelper.buildNextHeader(genesisHeaderDb)
      val createdF = blockHeaderDAO.create(blockHeader)

      val headerDbsF = createdF.flatMap(_ =>
        blockHeaderDAO.findClosestBeforeTime(
          UInt32(TimeUtil.currentEpochSecond)))

      headerDbsF.map { headerDbOpt =>
        assert(headerDbOpt.isDefined)
        assert(headerDbOpt.get.hashBE == blockHeader.hashBE)
      }
  }

  it must "retrieve the best chain tip saved in the database" in {
    blockHeaderDAO: BlockHeaderDAO =>
      val blockHeader = BlockHeaderHelper.buildNextHeader(genesisHeaderDb)

      val createdF = blockHeaderDAO.create(blockHeader)

      val chainTip1F = createdF.flatMap { _ =>
        blockHeaderDAO.getBestChainTips
      }

      val assert1F = chainTip1F.map { tips =>
        assert(tips.length == 1)
        assert(tips.head.blockHeader.hash == blockHeader.blockHeader.hash)
      }

      val blockHeader2 = BlockHeaderHelper.buildNextHeader(blockHeader)

      //insert another header and make sure that is the new last header
      assert1F.flatMap { _ =>
        val created2F = blockHeaderDAO.create(blockHeader2)
        val chainTip2F = created2F.flatMap(_ => blockHeaderDAO.getBestChainTips)

        chainTip2F.map { tips =>
          assert(tips.length == 1)
          assert(tips.head.blockHeader.hash == blockHeader2.blockHeader.hash)
        }
      }
  }

  it must "return the genesis block when retrieving block headers from an empty database" in {
    blockHeaderDAO: BlockHeaderDAO =>
      val chainTipsF = blockHeaderDAO.getBestChainTips
      chainTipsF.map { tips =>
        assert(tips.headOption == Some(genesisHeaderDb))
      }
  }

  it must "retrieve all chainTips in the last difficulty interval, not just the heaviest chain tip" in {
    blockHeaderDAO: BlockHeaderDAO =>
      val reorgFixtureF = buildBlockHeaderDAOCompetingHeaders(blockHeaderDAO)

      //now we have 2 competing tips, chainTips should return both competing headers
      val firstAssertionF = for {
        reorgFixture <- reorgFixtureF
        headerDb1 = reorgFixture.headerDb1
        headerDb2 = reorgFixture.headerDb2
        chainTips <- blockHeaderDAO.getForkedChainTips
      } yield {
        assert(chainTips.length == 2)
        assert(chainTips.contains(headerDb1))
        assert(chainTips.contains(headerDb2))
      }

      //ok, now we are going to build a new header off of headerDb1
      //however, headerDb2 is _still_ a possible chainTip that we can reorg
      //too. So we should still have both of them returned
      for {
        _ <- firstAssertionF
        reorgFixture <- reorgFixtureF
        headerD = BlockHeaderHelper.buildNextHeader(reorgFixture.headerDb1)
        _ <- reorgFixture.blockHeaderDAO.create(headerD)
        chainTips <- blockHeaderDAO.getForkedChainTips
      } yield {
        assert(chainTips.length == 2)
        assert(chainTips.contains(reorgFixture.headerDb1))
        assert(chainTips.contains(reorgFixture.headerDb2))
      }
  }

  it must "deduplicate blockchains so in reorg situations we do not return duplicates" in {
    blockHeaderDAO: BlockHeaderDAO =>
      val reorgFixtureF = buildBlockHeaderDAOCompetingHeaders(blockHeaderDAO)

      //now we have 2 competing tips, so we should return 2 chains
      val firstAssertionF = for {
        _ <- reorgFixtureF
        chains <- blockHeaderDAO.getBlockchains()
      } yield {
        assert(chains.length == 2)
      }

      firstAssertionF
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
          assert(headers.toSet.size == 2)
          assert(headers.toSet == Set(blockHeader, blockHeader1))
      }
  }

  it must "find a header with height 1" in { blockHeaderDAO: BlockHeaderDAO =>
    val blockHeader = BlockHeaderHelper.buildNextHeader(genesisHeaderDb)
    val createdF = blockHeaderDAO.create(blockHeader)

    val f: BlockHeaderDb => Boolean = { bh =>
      bh.height == 1
    }

    val foundF = createdF.flatMap(_ => blockHeaderDAO.find(f))

    for {
      created <- createdF
      found <- foundF
    } yield assert(found.get == created)
  }

  it must "get an ancestor at a specified height" in {
    blockHeaderDAO: BlockHeaderDAO =>
      val blockHeader = BlockHeaderHelper.buildNextHeader(genesisHeaderDb)
      val createdF = blockHeaderDAO.create(blockHeader)

      val genesisF = createdF.flatMap(created =>
        blockHeaderDAO.getAncestorAtHeight(created, 0))

      genesisF.map { genesisOpt =>
        assert(genesisOpt.contains(genesisHeaderDb))
      }
  }

  it must "implement getNAncestors correctly" in {
    blockHeaderDAO: BlockHeaderDAO =>
      val blockHeader = BlockHeaderHelper.buildNextHeader(genesisHeaderDb)
      val createdF = blockHeaderDAO.create(blockHeader)

      val noGenesisAncestorsF =
        blockHeaderDAO.getNAncestors(childHash =
                                       genesisHeaderDb.blockHeader.hashBE,
                                     n = 1)

      val foundGenesisF =
        blockHeaderDAO.getNAncestors(childHash =
                                       genesisHeaderDb.blockHeader.hashBE,
                                     n = 0)
      val emptyAssertion = for {
        noGenesisAncestors <- noGenesisAncestorsF
        foundGenesis <- foundGenesisF
      } yield {
        assert(noGenesisAncestors.length == 1)
        assert(noGenesisAncestors == Vector(ChainUnitTest.genesisHeaderDb))

        assert(foundGenesis.length == 1)
        assert(foundGenesis == Vector(ChainUnitTest.genesisHeaderDb))
      }

      val oneChildF = for {
        created <- createdF
        children <- blockHeaderDAO.getNAncestors(childHash =
                                                   created.blockHeader.hashBE,
                                                 n = 1)
      } yield {
        assert(children.length == 2)
        assert(children == Vector(created, genesisHeaderDb))
      }

      val bashHash = DoubleSha256DigestBE.empty
      val hashDoesNotExistF = for {
        children <- blockHeaderDAO.getNAncestors(childHash = bashHash, n = 1)
      } yield {
        assert(children.isEmpty)
      }

      for {
        _ <- emptyAssertion
        _ <- oneChildF
        lastAssert <- hashDoesNotExistF
      } yield lastAssert
  }

  it must "get the correct chain tip" in { blockerHeaderDAO: BlockHeaderDAO =>
    val db1 =
      BlockHeaderDbHelper.fromBlockHeader(
        1,
        BigInt(1, hex"fffef2bf0566ab".toArray),
        ChainTestUtil.blockHeader562462)

    val db2 =
      BlockHeaderDbHelper.fromBlockHeader(
        2,
        BigInt(1, hex"01253721228459eac00c".toArray),
        ChainTestUtil.blockHeader562463)

    for {
      _ <- blockerHeaderDAO.createAll(Vector(db1, db2))
      tips <- blockerHeaderDAO.getBestChainTips
    } yield assert(tips == Vector(db2))
  }

  it must "successfully map a max chain work block" in {
    blockerHeaderDAO: BlockHeaderDAO =>
      val bytes = ByteVector.fill(32)(0xff)
      val chainWork = BigInt(1, bytes.toArray)

      val db =
        BlockHeaderDbHelper.fromBlockHeader(1,
                                            chainWork,
                                            ChainTestUtil.blockHeader562462)

      for {
        _ <- blockerHeaderDAO.create(db)
        tips <- blockerHeaderDAO.getBestChainTips
      } yield assert(tips == Vector(db))
  }

  it must "get blockchains from the genesis header" in {
    blockHeaderDAO: BlockHeaderDAO =>
      val blockchainsF = blockHeaderDAO.getBlockchains()
      for {
        blockchains <- blockchainsF
      } yield {
        assert(blockchains.length == 1)
      }
  }

  it must "successfully getBlockchainsBetweenHeights" in {
    blockerHeaderDAO: BlockHeaderDAO =>
      val duplicate3 = BlockHeader(
        version = Int32.one,
        previousBlockHash = ChainTestUtil.blockHeader562463.hash,
        merkleRootHash = DoubleSha256Digest.empty,
        time = UInt32.zero,
        nBits = ChainTestUtil.blockHeader562464.nBits,
        nonce = UInt32.zero
      )

      val duplicate2 = BlockHeader(
        version = Int32.one,
        previousBlockHash = ChainTestUtil.blockHeader562462.hash,
        merkleRootHash = DoubleSha256Digest.empty,
        time = UInt32.zero,
        nBits = ChainTestUtil.blockHeader562463.nBits,
        nonce = UInt32.zero
      )

      val duplicate1 = BlockHeader(
        version = Int32.one,
        previousBlockHash = genesisHeaderDb.hashBE.flip,
        merkleRootHash = DoubleSha256Digest.empty,
        time = UInt32.zero,
        nBits = ChainTestUtil.blockHeader562462.nBits,
        nonce = UInt32.zero
      )

      val chain1 = Vector(
        BlockHeaderDbHelper.fromBlockHeader(3,
                                            BigInt(2),
                                            ChainTestUtil.blockHeader562464),
        BlockHeaderDbHelper.fromBlockHeader(2,
                                            BigInt(1),
                                            ChainTestUtil.blockHeader562463),
        BlockHeaderDbHelper.fromBlockHeader(1,
                                            BigInt(0),
                                            ChainTestUtil.blockHeader562462)
      )

      val chain2 = Vector(
        BlockHeaderDbHelper.fromBlockHeader(2, BigInt(1), duplicate2),
        BlockHeaderDbHelper.fromBlockHeader(1,
                                            BigInt(0),
                                            ChainTestUtil.blockHeader562462)
      )

      val chain3 = Vector(
        BlockHeaderDbHelper.fromBlockHeader(3, BigInt(2), duplicate3),
        BlockHeaderDbHelper.fromBlockHeader(2,
                                            BigInt(1),
                                            ChainTestUtil.blockHeader562463),
        BlockHeaderDbHelper.fromBlockHeader(1,
                                            BigInt(0),
                                            ChainTestUtil.blockHeader562462)
      )

      val chain4 = Vector(
        BlockHeaderDbHelper.fromBlockHeader(1, BigInt(0), duplicate1)
      )

      val expectedChains =
        Vector(Blockchain(chain1),
               Blockchain(chain2),
               Blockchain(chain3),
               Blockchain(chain4))

      val headers = expectedChains.flatMap(_.headers).distinct

      for {
        _ <- blockerHeaderDAO.createAll(headers)
        chains <- blockerHeaderDAO.getBlockchainsBetweenHeights(1, 3)
      } yield {
        assert(chains.nonEmpty)
        assert(expectedChains.forall(chains.contains))
      }
  }
}
