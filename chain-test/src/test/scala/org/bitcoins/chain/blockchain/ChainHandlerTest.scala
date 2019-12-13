package org.bitcoins.chain.blockchain

import akka.actor.ActorSystem
import org.bitcoins.chain.api.ChainApi
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.{
  BlockHeaderDb,
  BlockHeaderDbHelper,
  CompactFilterDb
}
import org.bitcoins.core.crypto.{
  DoubleSha256Digest,
  DoubleSha256DigestBE,
  ECPrivateKey
}
import org.bitcoins.core.gcs.{BlockFilter, FilterHeader, FilterType}
import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.p2p.CompactFilterMessage
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.core.protocol.{BitcoinAddress, BlockStamp}
import org.bitcoins.core.util.CryptoUtil
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.chain.fixture.ChainFixtureTag
import org.bitcoins.testkit.chain.{
  BlockHeaderHelper,
  ChainTestUtil,
  ChainUnitTest
}
import org.bitcoins.testkit.util.{FileUtil, ScalaTestUtil}
import org.scalatest.{Assertion, FutureOutcome}
import play.api.libs.json.Json

import scala.concurrent.Future

class ChainHandlerTest extends ChainUnitTest {

  override type FixtureParam = ChainHandler

  implicit override val system = ActorSystem("ChainUnitTest")

  // we're working with mainnet data
  implicit override lazy val appConfig: ChainAppConfig = {
    import BitcoinSTestAppConfig.ProjectType

    val memoryDb =
      BitcoinSTestAppConfig.configWithMemoryDb(Some(ProjectType.Chain))
    mainnetAppConfig.withOverrides(memoryDb)
  }

  val source = FileUtil.getFileAsSource("block_headers.json")
  val arrStr = source.getLines.next
  source.close()

  import org.bitcoins.rpc.serializers.JsonReaders.BlockHeaderReads

  val headersResult: Vector[BlockHeader] =
    Json.parse(arrStr).validate[Vector[BlockHeader]].get

  override val defaultTag: ChainFixtureTag = ChainFixtureTag.GenisisChainHandler

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withChainHandler(test)

  val genesis = ChainUnitTest.genesisHeaderDb
  behavior of "ChainHandler"

  it must "process a new valid block header, and then be able to fetch that header" in {
    chainHandler: ChainHandler =>
      val newValidHeader =
        BlockHeaderHelper.buildNextHeader(ChainUnitTest.genesisHeaderDb)
      val processedHeaderF =
        chainHandler.processHeader(newValidHeader.blockHeader)

      val foundHeaderF =
        processedHeaderF.flatMap(_.getHeader(newValidHeader.hashBE))

      foundHeaderF.map(found => assert(found.get == newValidHeader))
  }

  it must "have an in-order seed" in { _ =>
    val source = FileUtil.getFileAsSource("block_headers.json")
    val arrStr = source.getLines.next
    source.close()

    import org.bitcoins.rpc.serializers.JsonReaders.BlockHeaderReads
    val headersResult = Json.parse(arrStr).validate[Vector[BlockHeader]]
    if (headersResult.isError) {
      fail(headersResult.toString)
    }

    val blockHeaders = headersResult.get

    blockHeaders.reduce[BlockHeader] {
      case (prev, next) =>
        assert(next.previousBlockHashBE == prev.hashBE)
        next
    }

    succeed
  }

  it must "be able to process and fetch real headers from mainnet" in {
    chainHandler: ChainHandler =>
      val blockHeaders =
        headersResult.drop(
          ChainUnitTest.FIRST_POW_CHANGE - ChainUnitTest.FIRST_BLOCK_HEIGHT)

      val firstBlockHeaderDb =
        BlockHeaderDbHelper.fromBlockHeader(ChainUnitTest.FIRST_POW_CHANGE - 2,
                                            ChainTestUtil.blockHeader562462)

      val secondBlockHeaderDb =
        BlockHeaderDbHelper.fromBlockHeader(ChainUnitTest.FIRST_POW_CHANGE - 1,
                                            ChainTestUtil.blockHeader562463)

      val thirdBlockHeaderDb =
        BlockHeaderDbHelper.fromBlockHeader(ChainUnitTest.FIRST_POW_CHANGE,
                                            ChainTestUtil.blockHeader562464)

      /*
       * We need to insert one block before the first POW check because it is used on the next
       * POW check. We then need to insert the next to blocks to circumvent a POW check since
       * that would require we have an old block in the Blockchain that we don't have.
       */
      val firstThreeBlocks =
        Vector(firstBlockHeaderDb, secondBlockHeaderDb, thirdBlockHeaderDb)

      val createdF = chainHandler.blockHeaderDAO.createAll(firstThreeBlocks)

      createdF.flatMap { _ =>
        val blockchain = Blockchain.fromHeaders(firstThreeBlocks.reverse)
        val handler = ChainHandler(chainHandler.blockHeaderDAO,
                                   chainHandler.filterHeaderDAO,
                                   chainHandler.filterDAO,
                                   blockchain)
        val processorF = Future.successful(handler)
        // Takes way too long to do all blocks
        val blockHeadersToTest = blockHeaders.tail
          .take(
            (2 * chainHandler.chainConfig.chain.difficultyChangeInterval + 1).toInt)

        processHeaders(processorF = processorF,
                       headers = blockHeadersToTest,
                       height = ChainUnitTest.FIRST_POW_CHANGE + 1)
      }
  }

  it must "benchmark ChainHandler.processHeaders()" in {
    chainHandler: ChainHandler =>
      val blockHeaders =
        headersResult.drop(
          ChainUnitTest.FIRST_POW_CHANGE - ChainUnitTest.FIRST_BLOCK_HEIGHT)

      val firstBlockHeaderDb =
        BlockHeaderDbHelper.fromBlockHeader(ChainUnitTest.FIRST_POW_CHANGE - 2,
                                            ChainTestUtil.blockHeader562462)

      val secondBlockHeaderDb =
        BlockHeaderDbHelper.fromBlockHeader(ChainUnitTest.FIRST_POW_CHANGE - 1,
                                            ChainTestUtil.blockHeader562463)

      val thirdBlockHeaderDb =
        BlockHeaderDbHelper.fromBlockHeader(ChainUnitTest.FIRST_POW_CHANGE,
                                            ChainTestUtil.blockHeader562464)

      /*
       * We need to insert one block before the first POW check because it is used on the next
       * POW check. We then need to insert the next to blocks to circumvent a POW check since
       * that would require we have an old block in the Blockchain that we don't have.
       */
      val firstThreeBlocks =
        Vector(firstBlockHeaderDb, secondBlockHeaderDb, thirdBlockHeaderDb)

      val createdF = chainHandler.blockHeaderDAO.createAll(firstThreeBlocks)

      createdF.flatMap { _ =>
        val blockchain = Blockchain.fromHeaders(firstThreeBlocks.reverse)
        val handler = chainHandler.copy(blockchains = Vector(blockchain))

        // Takes way too long to do all blocks
        val blockHeadersToTest = blockHeaders.tail
          .take(
            (2 * chainHandler.chainConfig.chain.difficultyChangeInterval + 1))

        val processedF = handler.processHeaders(blockHeadersToTest)

        for {
          ch <- processedF
          bestHash <- ch.getBestBlockHash
        } yield assert(bestHash == blockHeadersToTest.last.hashBE)
      }
  }

  it must "handle a very basic reorg where one chain is one block behind the best chain" in {
    chainHandler: ChainHandler =>
      val reorgFixtureF = buildChainHandlerCompetingHeaders(chainHandler)
      val chainHandlerCF = reorgFixtureF.map(_.chainApi)
      // header B, best hash ATM
      val newHeaderBF = reorgFixtureF.map(_.headerDb1)
      // header C, same height as B but was seen later
      val newHeaderCF = reorgFixtureF.map(_.headerDb2)

      // check that header B is the leader
      val assertBBestHashF = for {
        chainHandler <- chainHandlerCF
        headerB <- newHeaderBF
        bestHash <- chainHandler.getBestBlockHash
      } yield {
        assert(bestHash == headerB.hashBE)
      }

      // build a new header D off of C which was seen later
      // but has the same height as B
      val newHeaderDF =
        newHeaderCF.map(h => BlockHeaderHelper.buildNextHeader(h))

      val chainHandlerDF = for {
        _ <- assertBBestHashF
        newHeaderD <- newHeaderDF
        chainHandler <- chainHandlerCF
        chainHandlerD <- chainHandler.processHeader(newHeaderD.blockHeader)
      } yield chainHandlerD

      for {
        chainHandler <- chainHandlerDF
        newHeaderD <- newHeaderDF
        hash <- chainHandler.getBestBlockHash
      } yield {
        // assert that header D overtook header B
        assert(hash == newHeaderD.hashBE)
      }
  }

  it must "NOT reorg to a shorter chain that just received a new block" in {
    chainHandler: ChainHandler =>
      val reorgFixtureF = buildChainHandlerCompetingHeaders(chainHandler)
      val chainHandlerCF = reorgFixtureF.map(_.chainApi)
      val newHeaderBF = reorgFixtureF.map(_.headerDb1)
      val newHeaderCF = reorgFixtureF.map(_.headerDb2)

      //we are going to generate two new blocks on chain C
      val chainHandlerEWithHeaderF: Future[(ChainApi, BlockHeaderDb)] = for {
        newHeaderC <- newHeaderCF
        chainHandler <- chainHandlerCF
        headerD = BlockHeaderHelper.buildNextHeader(newHeaderC)
        headerE = BlockHeaderHelper.buildNextHeader(headerD)
        chainHandlerE <- chainHandler.processHeaders(
          Vector(headerD.blockHeader, headerE.blockHeader))
      } yield (chainHandlerE, headerE)

      val chainHandlerEF = chainHandlerEWithHeaderF.map(_._1)

      val headerEF = chainHandlerEWithHeaderF.map(_._2)
      //now we are going to attempt to generate a block on top of B
      //we should _not_ reorg to a new best tip after adding block F ontop of B
      //the best hash should still be header E's best hash.

      val chainHandlerFF = for {
        chainHandler <- chainHandlerEF
        headerB <- newHeaderBF
        headerF = BlockHeaderHelper.buildNextHeader(headerB)
        chainHandlerF <- chainHandler.processHeader(headerF.blockHeader)
      } yield chainHandlerF

      for {
        chainHandlerF <- chainHandlerFF
        headerE <- headerEF
        bestHash <- chainHandlerF.getBestBlockHash
      } yield assert(bestHash == headerE.hashBE)
  }

  it must "get the highest filter header" in { chainHandler: ChainHandler =>
    {
      for {
        count <- chainHandler.getFilterHeaderCount
        genesisFilterHeader <- chainHandler.getFilterHeadersAtHeight(count)
      } yield {
        assert(genesisFilterHeader.size == 1)
        assert(
          genesisFilterHeader.contains(ChainUnitTest.genesisFilterHeaderDb))
        assert(count == 0)
      }
    }
  }

  it must "NOT create a filter header for an unknown block" in {
    chainHandler: ChainHandler =>
      {
        val firstFilterHeader = FilterHeader(
          filterHash =
            DoubleSha256Digest.fromBytes(ECPrivateKey.freshPrivateKey.bytes),
          prevHeaderHash = DoubleSha256Digest.empty)
        val newChainHandlerF = chainHandler.processFilterHeader(
          firstFilterHeader,
          DoubleSha256DigestBE.fromBytes(ECPrivateKey.freshPrivateKey.bytes))
        recoverToSucceededIf[UnknownBlockHash](newChainHandlerF)
      }
  }

  it must "get the highest filter" in { chainHandler: ChainHandler =>
    {
      for {
        count <- chainHandler.getFilterCount
        genesisFilter <- chainHandler.getFiltersAtHeight(count)
      } yield {
        assert(count == 0)
        assert(genesisFilter.contains(ChainUnitTest.genesisFilterDb))
      }
    }
  }

  it must "NOT create an unknown filter" in { chainHandler: ChainHandler =>
    {
      val blockHeader =
        BlockHeader(
          version = Int32(1),
          previousBlockHash = ChainUnitTest.genesisHeaderDb.hashBE.flip,
          merkleRootHash = DoubleSha256Digest.empty,
          time = UInt32(1231006505),
          nBits = UInt32(545259519),
          nonce = UInt32(2083236893)
        )
      val unknownHashF = for {
        _ <- chainHandler.processHeader(blockHeader)
        blockHashBE <- chainHandler.getHeadersAtHeight(1).map(_.head.hashBE)
        golombFilter = BlockFilter.fromHex("017fa880", blockHashBE.flip)
        firstFilter = CompactFilterMessage(blockHash = blockHashBE.flip,
                                           filter = golombFilter)
        firstFilterHeader = FilterHeader(
          filterHash =
            DoubleSha256Digest.fromBytes(ECPrivateKey.freshPrivateKey.bytes),
          prevHeaderHash = ChainUnitTest.genesisFilterHeaderDb.hashBE.flip)
        newChainHandler <- chainHandler.processFilterHeader(firstFilterHeader,
                                                            blockHashBE)
        process <- newChainHandler.processFilter(firstFilter)
      } yield {
        process
      }
      recoverToSucceededIf[UnknownFilterHash](unknownHashF)
    }
  }

  it must "NOT create a filter of an unknown block" in {
    chainHandler: ChainHandler =>
      {
        val blockHashBE =
          DoubleSha256DigestBE.fromBytes(ECPrivateKey.freshPrivateKey.bytes)
        val golombFilter = BlockFilter.fromHex("017fa880", blockHashBE.flip)
        val firstFilterHeader = FilterHeader(filterHash = golombFilter.hash,
                                             prevHeaderHash =
                                               DoubleSha256Digest.empty)
        val unknownBlockF = for {
          realBlockHashBE <- chainHandler
            .getHeadersAtHeight(0)
            .map(_.head.hashBE)
          newChainHandler <- chainHandler.processFilterHeader(firstFilterHeader,
                                                              blockHashBE)
        } yield {
          assert(realBlockHashBE != blockHashBE)
          newChainHandler
        }
        recoverToSucceededIf[UnknownBlockHash](unknownBlockF)
      }
  }

  it must "create a filter checkpoint map" in { chainHandler: ChainHandler =>
    for {
      realBlockHashBE <- chainHandler.getHeadersAtHeight(0).map(_.head.hashBE)
      filterHashBE = DoubleSha256DigestBE.fromBytes(
        ECPrivateKey.freshPrivateKey.bytes)
      newChainHandler <- chainHandler.processCheckpoint(filterHashBE,
                                                        realBlockHashBE)
    } yield {
      assert(
        newChainHandler
          .asInstanceOf[ChainHandler]
          .blockFilterCheckpoints == Map(realBlockHashBE -> filterHashBE))
    }
  }

  it must "generate a range for a block filter query" in {
    chainHandler: ChainHandler =>
      for {
        bestBlock <- chainHandler.getBestBlockHeader()
        bestBlockHashBE = bestBlock.hashBE
        rangeOpt <- chainHandler.nextHeaderBatchRange(
          DoubleSha256DigestBE.empty,
          1)
      } yield {
        assert(rangeOpt.nonEmpty)
        assert(rangeOpt.get._1 == 0)
        assert(rangeOpt.get._2 == bestBlockHashBE.flip)
      }
  }

  it must "generate a range for a block filter header query" in {
    chainHandler: ChainHandler =>
      for {
        bestBlock <- chainHandler.getBestBlockHeader()
        bestBlockHashBE = bestBlock.hashBE
        rangeOpt <- chainHandler.nextFilterHeaderBatchRange(
          DoubleSha256DigestBE.empty,
          1)
      } yield {
        assert(rangeOpt.nonEmpty)
        assert(rangeOpt.get._1 == 0)
        assert(rangeOpt.get._2 == bestBlockHashBE.flip)
      }
  }

  it must "return the number of confirmations" in {
    chainHandler: ChainHandler =>
      for {
        bestBlockHashBE <- chainHandler.getBestBlockHash()
        confirmations <- chainHandler.getNumberOfConfirmations(bestBlockHashBE)
      } yield {
        assert(confirmations == Some(1))
      }
  }

  it must "return the height by block stamp" in { chainHandler: ChainHandler =>
    for {
      bestBlock <- chainHandler.getBestBlockHeader()
      stamp1 = BlockStamp.BlockHash(bestBlock.hashBE)
      stamp2 = BlockStamp.BlockHeight(bestBlock.height)
      stamp3 = BlockStamp.BlockTime(bestBlock.time)
      height1 <- chainHandler.getHeightByBlockStamp(stamp1)
      height2 <- chainHandler.getHeightByBlockStamp(stamp2)
      // TODO implement BlockTime
//      height3 <- chainHandler.getHeightByBlockStamp(stamp3)
    } yield {
      assert(height1 == height2)
//      assert(height1 == height3)
    }
  }

  it must "match block filters" in { chainHandler: ChainHandler =>
    import scodec.bits._

    // This is a filter for a random block on testnet
    val filterBytes: ByteVector =
      hex"fd2701f0ed169ad16107a8a74609b9e4de3c6133c564f79923ca228805d3" ++
        hex"8e3efc796c4b35034cb573b10b759cdda5efd19e1cdb4d343afcb06455fa" ++
        hex"820b06eca828ad61d3377fa464f3bd06ff4432310a363f667e13d09ba993" ++
        hex"264c703a0aa668b33eaa555bd3e93ac85dfde380ab723aafd407dfa13ffe" ++
        hex"2e7ddf6f452bd0d977617c4ab2dc3b38c26810023984ad57890e3cf34cfc" ++
        hex"2d4a6973b9430ede26bfd9f5bb24e043d48483d84b9025d0a940b15f13fc" ++
        hex"0a1e77abd7626869f417c7710e9a6315477691d7c4e2c50f0e776755a62a" ++
        hex"b6f0e8eb7a3be8d1a8c3d9dd4602efc5146f0d431d1669378d7afa03c7b9" ++
        hex"84d9b0b78007abb6e7c036156e5186d1d79a2f37daecfcbe8821cf42851c" ++
        hex"b10ef0c359307d54e53078eb631f02c067a474dceb484da20bc0e7c5451a" ++
        hex"b957f46b306caa82938b19bb34fd76c5cc07e048932524704dec8f72c91c" ++
        hex"d5ee1f4648de839047a0bea0d4d4d66c19cfccc2b5f285a84af18114f608" ++
        hex"f144391648aedfb5ffcccbb51272512d6ba9a2e19a47cebe5b50a8a7073a" ++
        hex"1c24059440444047a41bdbab16f61bc4b0ee8987de82fd25cc62abc86e2b" ++
        hex"577fc55175be138680df7253a8bcae9d9954391d3bed806ce5a6869b4553" ++
        hex"0f214486b1b7f0347efcfde58ca0882f059f7b1541c74506930897c78e23" ++
        hex"a6c94b49856369606ed652b8c7402a49f289cb5d1098bb999112225327e0" ++
        hex"a32efd2bcd192a2ffbd1997c6a3b7d1a9445bc31fb57485ebe0c431e482b" ++
        hex"04e509e557cff107cee08a45c22aa3cbdcb9d305bd95c919e90239e0ec29" ++
        hex"2a5418a6151f431e8ab82278b3d816ecd483f43d3d657dae9996cc523fdd" ++
        hex"242c4e01935db91a2936e9398ff7278b8a3430eed99ad25fc2a41afc0b4a" ++
        hex"e417f6c1785414607cfa13f04173740333a5b58655c74a51deddb38cf8c3" ++
        hex"d50b7d2ccf380cad34a5c341e7155494cc4560dff3b19bf88b4d73e9ce76" ++
        hex"cbeff573fe93674e4a752d06d5321ff00a4582d62683fb4986d36eaec825" ++
        hex"c14d41b2d5aefaf539e989f7fa097eac657c70b975c56e26b73fb9401ce3" ++
        hex"81502f0883d52c6a3bcc956e0ea1787f0717d0205fecfe55b01edb1ac0"

    val compactFilterDb = CompactFilterDb(
      hashBE = CryptoUtil.doubleSHA256(filterBytes).flip,
      filterType = FilterType.Basic,
      bytes = filterBytes,
      height = 1,
      // this is the hash of the random testnet block
      blockHashBE = DoubleSha256DigestBE
        .fromHex(
          "00000000496dcc754fabd97f3e2df0a7337eab417d75537fecf97a7ebb0e7c75")
    )
    for {
      created <- chainHandler.filterDAO.create(compactFilterDb)
      matched <- chainHandler.getMatchingBlocks(
        scripts = Vector(
          // this is a random address which is included into the block
          BitcoinAddress("n1RH2x3b3ah4TGQtgrmNAHfmad9wr8U2QY").get.scriptPubKey),
        startOpt = None,
        endOpt = None
      )(system.dispatcher)
    } yield {
      assert(Vector(created.blockHashBE) == matched)
    }
  }

  final def processHeaders(
      processorF: Future[ChainApi],
      headers: Vector[BlockHeader],
      height: Int): Future[Assertion] = {
    val processedHeadersF = processorF.flatMap(_.processHeaders(headers))

    def loop(
        remainingHeaders: Vector[BlockHeader],
        height: Int,
        accum: Vector[Future[Assertion]]): Vector[Future[Assertion]] = {
      remainingHeaders match {
        case header +: headersTail =>
          val getHeaderF = processedHeadersF.flatMap(_.getHeader(header.hashBE))
          val expectedBlockHeaderDb =
            BlockHeaderDbHelper.fromBlockHeader(height, header)
          val assertionF =
            getHeaderF.map(headerOpt =>
              assert(headerOpt.contains(expectedBlockHeaderDb)))
          val newAccum = accum.:+(assertionF)
          loop(headersTail, height + 1, newAccum)
        case Vector() =>
          accum
      }
    }

    val vecFutAssert: Vector[Future[Assertion]] =
      loop(headers, height, Vector.empty)

    ScalaTestUtil.toAssertF(vecFutAssert)
  }

  /** Builds two competing headers that are built from the same parent */
  private def buildCompetingHeaders(
      parent: BlockHeaderDb): (BlockHeader, BlockHeader) = {
    val newHeaderB =
      BlockHeaderHelper.buildNextHeader(parent)

    val newHeaderC =
      BlockHeaderHelper.buildNextHeader(parent)

    (newHeaderB.blockHeader, newHeaderC.blockHeader)
  }

  case class ReorgFixture(
      chainApi: ChainApi,
      headerDb1: BlockHeaderDb,
      headerDb2: BlockHeaderDb,
      oldBestBlockHeader: BlockHeaderDb) {
    lazy val header1: BlockHeader = headerDb1.blockHeader
    lazy val header2: BlockHeader = headerDb2.blockHeader
  }

  /** Builds two competing headers off of the [[ChainHandler.getBestBlockHash best chain tip]] */
  private def buildChainHandlerCompetingHeaders(
      chainHandler: ChainHandler): Future[ReorgFixture] = {
    for {
      oldBestTip <- chainHandler.getBestBlockHeader()
      (newHeaderB, newHeaderC) = buildCompetingHeaders(oldBestTip)
      newChainApi <- chainHandler.processHeaders(Vector(newHeaderB, newHeaderC))
      newHeaderDbB <- newChainApi.getHeader(newHeaderB.hashBE)
      newHeaderDbC <- newChainApi.getHeader(newHeaderC.hashBE)
    } yield {
      ReorgFixture(newChainApi, newHeaderDbB.get, newHeaderDbC.get, oldBestTip)
    }
  }
}
