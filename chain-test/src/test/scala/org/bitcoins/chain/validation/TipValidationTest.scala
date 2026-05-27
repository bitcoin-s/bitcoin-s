package org.bitcoins.chain.validation

import org.bitcoins.chain.models.BlockHeaderDAO
import org.bitcoins.core.api.chain.{
  Blockchain,
  ConnectTipResult,
  TipUpdateResult
}
import org.bitcoins.core.api.chain.db.{BlockHeaderDb, BlockHeaderDbHelper}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.blockchain.{
  BlockHeader,
  RegTestNetChainParams
}
import org.bitcoins.testkit.chain.{BlockHeaderHelper, ChainUnitTest}
import org.bitcoins.testkitcore.chain.ChainTestUtil
import org.scalatest.FutureOutcome

import java.time.Instant
import scala.concurrent.Future

class TipValidationTest extends ChainUnitTest {

  behavior of "TipValidation"
  override type FixtureParam = BlockHeaderDAO
  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withBlockHeaderDAO(test)

  it must "reject a new tip whose time is <= the median of the last 11 headers" in {
    blockHeaderDAO =>
      val firstHeaderF = blockHeaderDAO.getBestChainTips.map(_.head)
      val headersDbF: Future[Seq[BlockHeaderDb]] = firstHeaderF.map { f =>
        // produce 11 headers after `f` (inclusive of f we get 12 total)
        // adjust the range if you want exactly 11 headers total
        (0 until 11).foldLeft(Vector(f)) { (prevHeaders, _) =>
          val nextHeader = BlockHeaderHelper.buildNextHeader(prevHeaders.last)
          prevHeaders.appended(nextHeader)
        }
      }
      val blockchainF = headersDbF.map(_.reverse).map(Blockchain.fromHeaders)
      for {
        blockchain <- blockchainF
        medianTimePast = blockchain.getMedianTimePast
        newHeaderBadMTP = BlockHeaderHelper
          .buildNextHeader(blockchain.tip,
                           timeOpt = medianTimePast.map(m => UInt32(m - 1)))
          .blockHeader
        newHeaderStaleMTP = BlockHeaderHelper
          .buildNextHeader(blockchain.tip,
                           timeOpt = medianTimePast.map(UInt32.apply))
          .blockHeader
        newHeaderGoodMTP = BlockHeaderHelper
          .buildNextHeader(blockchain.tip,
                           timeOpt = medianTimePast.map(m => UInt32(m + 1)))
          .blockHeader
        tipValidationResultBadMTP = TipValidation.checkNewTip(
          newHeaderBadMTP,
          blockchain,
          chainParams
        )
        tipValidationResultStaleMTP = TipValidation.checkNewTip(
          newHeaderStaleMTP,
          blockchain,
          chainParams
        )
        tipValidationResultGoodMTP = TipValidation.checkNewTip(
          newHeaderGoodMTP,
          blockchain,
          chainParams
        )
      } yield {
        assert(
          tipValidationResultBadMTP == TipUpdateResult.BadMTP(newHeaderBadMTP)
        )
        assert(
          tipValidationResultStaleMTP == TipUpdateResult.BadMTP(
            newHeaderStaleMTP))
        val goodDb = BlockHeaderDbHelper.fromBlockHeader(
          blockchain.tip.height + 1,
          blockchain.tip.chainWork + org.bitcoins.chain.pow.Pow
            .getBlockProof(newHeaderGoodMTP),
          newHeaderGoodMTP
        )
        assert(tipValidationResultGoodMTP == TipUpdateResult.Success(goodDb))
      }
  }

  it must "reject block headers that are too new" in { blockHeaderDAO =>
    val blockchainF = blockHeaderDAO.getBlockchains().map(_.head)
    for {
      blockchain <- blockchainF

      // 2 hours and 5 second in the future
      tooNewTime = UInt32(Instant.now().toEpochMilli / 1000 + (60 * 60 * 2 + 5))
      // just under 2 hours in the future
      validTime = UInt32(Instant.now().toEpochMilli / 1000 + (60 * 60 * 2 - 1))
      newHeaderTooNew = BlockHeaderHelper
        .buildNextHeader(blockchain.tip, timeOpt = Some(tooNewTime))
        .blockHeader

      newHeaderTime2hoursDb = BlockHeaderHelper
        .buildNextHeader(blockchain.tip, timeOpt = Some(validTime))
      newHeaderTime2hours = newHeaderTime2hoursDb.blockHeader
      tipValidationResultTooNew = TipValidation.checkNewTip(
        newHeaderTooNew,
        blockchain,
        chainParams
      )
      tipValidationResultValid = TipValidation.checkNewTip(
        newHeaderTime2hours,
        blockchain,
        chainParams
      )
    } yield {
      assert(
        tipValidationResultTooNew == TipUpdateResult.TimeTooNew(newHeaderTooNew)
      )
      assert(
        tipValidationResultValid == TipUpdateResult.Success(
          newHeaderTime2hoursDb)
      )
    }
  }

  it must "correctly identify a bad tip" in { _ =>
    val genesis = ChainTestUtil.regTestGenesisHeaderDb
    val chain = Blockchain(Vector(genesis))

    val goodHeader = BlockHeaderHelper.buildNextHeader(genesis).blockHeader
    val badHeader = BlockHeader(
      version = goodHeader.version,
      previousBlockHash = goodHeader.previousBlockHash,
      merkleRootHash = goodHeader.merkleRootHash,
      time = goodHeader.time,
      nBits = UInt32.zero,
      nonce = goodHeader.nonce
    )

    val result = TipValidation.connectTip(header = badHeader,
                                          blockchain = chain,
                                          chainParams = RegTestNetChainParams)

    assert(result.isInstanceOf[ConnectTipResult.BadTip])
  }

  it must "return Some(mtp) for a header that is in the chain with >= 11 ancestors" in {
    _ =>
      // Build genesis + 11 successors (12 total) so the tip has exactly 11
      // ancestors available — the minimum required by nMedianTimeSpan
      val genesis = ChainTestUtil.regTestGenesisHeaderDb
      val headers =
        (0 until Blockchain.nMedianTimeSpan).foldLeft(Vector(genesis)) {
          (acc, _) =>
            acc :+ BlockHeaderHelper.buildNextHeader(acc.last)
        }
      // Blockchain.fromHeaders expects headers in descending height order
      val blockchain = Blockchain.fromHeaders(headers.reverse)
      val tip = blockchain.tip

      val result = blockchain.getMedianTimePast(tip)
      assert(result.isDefined, "Expected Some(mtp) for a fully-populated chain")

      // The MTP is the median of the 11 timestamps starting at the tip; verify
      // it sits within the time range of those 11 headers
      val window = headers.reverse
        .take(Blockchain.nMedianTimeSpan)
        .map(_.time.toLong)
        .sorted
      val expectedMtp = window(window.length / 2)
      assert(result.contains(expectedMtp))
  }

  it must "return None when the requested header is not present in the blockchain" in {
    _ =>
      val genesis = ChainTestUtil.regTestGenesisHeaderDb
      val headers =
        (0 until Blockchain.nMedianTimeSpan).foldLeft(Vector(genesis)) {
          (acc, _) =>
            acc :+ BlockHeaderHelper.buildNextHeader(acc.last)
        }
      val blockchain = Blockchain.fromHeaders(headers.reverse)

      // Build a header that is NOT part of the chain
      val outsideHeader = BlockHeaderHelper.buildNextHeader(genesis)

      val result = blockchain.getMedianTimePast(outsideHeader)
      assert(
        result.isEmpty,
        "Expected None when the header is not found in the blockchain"
      )
  }

  it must "return None when the blockchain does not have enough headers below the requested header" in {
    _ =>
      // Build genesis + 9 successors (10 total) — fewer than nMedianTimeSpan=11
      val genesis = ChainTestUtil.regTestGenesisHeaderDb
      val headers = (0 until 9).foldLeft(Vector(genesis)) { (acc, _) =>
        acc :+ BlockHeaderHelper.buildNextHeader(acc.last)
      }
      val blockchain = Blockchain.fromHeaders(headers.reverse)
      val tip = blockchain.tip

      assert(
        blockchain.headers.length < Blockchain.nMedianTimeSpan,
        "Pre-condition: chain must have fewer headers than nMedianTimeSpan"
      )

      val result = blockchain.getMedianTimePast(tip)
      assert(
        result.isEmpty,
        "Expected None when there are fewer than 11 headers available for MTP"
      )
  }

  it must "return Some(mtp) for a non-tip header in the middle of the blockchain" in {
    _ =>
      // Build genesis + 15 successors (16 total)
      val genesis = ChainTestUtil.regTestGenesisHeaderDb
      val headers = (0 until 15).foldLeft(Vector(genesis)) { (acc, _) =>
        acc :+ BlockHeaderHelper.buildNextHeader(acc.last)
      }
      // Blockchain stores headers in descending order (tip first)
      val descHeaders = headers.reverse
      val blockchain = Blockchain.fromHeaders(descHeaders)

      // Pick a header that sits in the middle (index 5 from the top, height 10)
      val midHeader = descHeaders(5)
      assert(
        descHeaders.length - 5 >= Blockchain.nMedianTimeSpan,
        "Pre-condition: must have >= 11 headers from midHeader downward"
      )

      val result = blockchain.getMedianTimePast(midHeader)
      assert(
        result.isDefined,
        s"Expected Some(mtp) for header at height=${midHeader.height}"
      )

      // Verify it matches computing MTP manually from that slice
      val slicedBlockchain = Blockchain.fromHeaders(descHeaders.drop(5))
      val expected = slicedBlockchain.getMedianTimePast
      assert(result == expected)
  }

  it must "return None for a mid-chain header when insufficient ancestors exist below it" in {
    _ =>
      // Genesis + 12 successors (13 total). The header at index 10 from the top
      // (height 2) only has 3 headers beneath it — fewer than 11.
      val genesis = ChainTestUtil.regTestGenesisHeaderDb
      val headers =
        (0 until Blockchain.nMedianTimeSpan + 1).foldLeft(Vector(genesis)) {
          (acc, _) =>
            acc :+ BlockHeaderHelper.buildNextHeader(acc.last)
        }
      val descHeaders = headers.reverse
      val blockchain = Blockchain.fromHeaders(descHeaders)

      // Header near the bottom of the chain (index 10 from tip == height 2)
      val nearBottomHeader = descHeaders(10)
      val remainingBelow = descHeaders.length - 10
      assert(
        remainingBelow < Blockchain.nMedianTimeSpan,
        s"Pre-condition: only $remainingBelow headers below the chosen header, need < ${Blockchain.nMedianTimeSpan}"
      )

      val result = blockchain.getMedianTimePast(nearBottomHeader)
      assert(
        result.isEmpty,
        "Expected None when not enough ancestors exist below the requested header"
      )
  }
  it must "connect a new header to the current tip of a blockchain" in { _ =>
    val blockchain = Blockchain.fromHeaders(
      headers = Vector(ChainTestUtil.regTestGenesisHeaderDb)
    )

    val newHeader =
      BlockHeaderHelper.buildNextHeader(ChainTestUtil.regTestGenesisHeaderDb)

    val connectTip =
      TipValidation.connectTip(header = newHeader.blockHeader,
                               blockchain = blockchain,
                               chainParams = RegTestNetChainParams)

    connectTip match {
      case ConnectTipResult.ExtendChain(_, newChain) =>
        assert(newHeader == newChain.tip)

      case _ @(_: ConnectTipResult.Reorg | _: ConnectTipResult.BadTip) =>
        fail()
    }
  }
}
