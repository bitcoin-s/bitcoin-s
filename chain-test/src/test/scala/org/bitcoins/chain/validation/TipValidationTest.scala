package org.bitcoins.chain.validation

import org.apache.pekko.stream.scaladsl.{Keep, Sink, Source}
import org.bitcoins.chain.blockchain.Blockchain
import org.bitcoins.chain.models.BlockHeaderDAO
import org.bitcoins.core.api.chain.db.{BlockHeaderDb, BlockHeaderDbHelper}
import org.bitcoins.core.number.UInt32
import org.bitcoins.testkit.chain.{BlockHeaderHelper, ChainDbUnitTest}
import org.scalatest.FutureOutcome

import java.time.Instant
import scala.concurrent.Future

class TipValidationTest extends ChainDbUnitTest {

  behavior of "TipValidation"
  override type FixtureParam = BlockHeaderDAO
  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withBlockHeaderDAO(test)

  it must "reject a new tip whose time is <= the median of the last 11 headers" in {
    blockHeaderDAO =>
      val firstHeaderF = blockHeaderDAO.getBestChainTips.map(_.head)
      val headersDbF: Future[Seq[BlockHeaderDb]] = firstHeaderF.flatMap { f =>
        Source(f.height.until(f.height + 11))
          .fold(Vector(f)) { (prevHeaders, _) =>
            val nextHeader = BlockHeaderHelper.buildNextHeader(prevHeaders.last)
            prevHeaders.appended(nextHeader)
          }
          .toMat(Sink.last)(Keep.right)
          .run()
      }
      val blockchainF = headersDbF.map(_.reverse).map(Blockchain.fromHeaders)
      for {
        blockchain <- blockchainF
        medianTimePast = blockchain.getMedianTimePast
        newHeaderBadMTP = BlockHeaderHelper
          .buildNextHeader(blockchain.tip,
                           timeOpt = Some(UInt32(medianTimePast - 1)))
          .blockHeader
        newHeaderStaleMTP = BlockHeaderHelper
          .buildNextHeader(blockchain.tip,
                           timeOpt = Some(UInt32(medianTimePast)))
          .blockHeader
        newHeaderGoodMTP = BlockHeaderHelper
          .buildNextHeader(blockchain.tip,
                           timeOpt = Some(UInt32(medianTimePast + 1)))
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

  it must "reject blocks headers that are too new" in { blockHeaderDAO =>
    val firstHeaderF: Future[BlockHeaderDb] =
      blockHeaderDAO.getBestChainTips.map(_.head)
    val blockchainF = firstHeaderF.map(b => Blockchain.fromHeaders(Vector(b)))
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
        tipValidationResultTooNew == TipUpdateResult.TimeToNew(newHeaderTooNew)
      )
      assert(
        tipValidationResultValid == TipUpdateResult.Success(
          newHeaderTime2hoursDb)
      )
    }
  }
}
