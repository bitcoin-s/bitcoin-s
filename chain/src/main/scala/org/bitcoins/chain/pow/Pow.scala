package org.bitcoins.chain.pow

import org.bitcoins.chain.models.{BlockHeaderDAO, BlockHeaderDb}
import org.bitcoins.core.number.UInt32
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.protocol.blockchain.{BlockHeader, ChainParams}
import org.bitcoins.core.util.NumberUtil

import scala.concurrent.{ExecutionContext, Future}

/**
  * Implements functions found inside of bitcoin core's
  * @see [[https://github.com/bitcoin/bitcoin/blob/35477e9e4e3f0f207ac6fa5764886b15bf9af8d0/src/pow.cpp pow.cpp]]
  */
sealed abstract class Pow {

  /**
    * Gets the next proof of work requirement for a block
    * @see [[https://github.com/bitcoin/bitcoin/blob/35477e9e4e3f0f207ac6fa5764886b15bf9af8d0/src/pow.cpp#L13 Mimics bitcoin core implmentation]]
    * @param tip
    * @param newPotentialTip
    * @return
    */
  def getNetworkWorkRequired(
      tip: BlockHeaderDb,
      newPotentialTip: BlockHeader,
      blockHeaderDAO: BlockHeaderDAO)(
      implicit ec: ExecutionContext,
      config: ChainAppConfig): Future[UInt32] = {
    val chainParams = config.chain
    val currentHeight = tip.height

    val powLimit = NumberUtil.targetCompression(bigInteger =
                                                  chainParams.powLimit,
                                                isNegative = false)
    if ((currentHeight + 1) % chainParams.difficultyChangeInterval != 0) {
      if (chainParams.allowMinDifficultyBlocks) {
        // Special difficulty rule for testnet:
        // If the new block's timestamp is more than 2* 10 minutes
        // then allow mining of a min-difficulty block.
        if (newPotentialTip.time.toLong > tip.blockHeader.time.toLong + chainParams.powTargetSpacing.toSeconds * 2) {
          Future.successful(powLimit)
        } else {
          // Return the last non-special-min-difficulty-rules-block
          //while (pindex->pprev && pindex->nHeight % params.DifficultyAdjustmentInterval() != 0 && pindex->nBits == nProofOfWorkLimit)
          //                    pindex = pindex->pprev;
          val nonMinDiffF = blockHeaderDAO.find { h =>
            h.nBits != powLimit || h.height % chainParams.difficultyChangeInterval == 0
          }

          nonMinDiffF.map {
            case Some(bh) => bh.nBits
            case None     =>
              //if we can't find a non min diffulty block, let's just fail
              throw new RuntimeException(
                s"Could not find non mindiffulty block in chain! hash=${tip.hashBE.hex} height=${currentHeight}")
          }
        }
      } else {
        Future.successful(tip.blockHeader.nBits)
      }
    } else {
      val firstHeight = currentHeight - (chainParams.difficultyChangeInterval - 1)

      require(firstHeight >= 0,
              s"We must have our first height be postive, got=${firstHeight}")

      val firstBlockAtIntervalF: Future[Option[BlockHeaderDb]] = {
        blockHeaderDAO.getAncestorAtHeight(tip, firstHeight)
      }

      firstBlockAtIntervalF.flatMap {
        case Some(firstBlock) =>
          calculateNextWorkRequired(currentTip = tip, firstBlock, chainParams)
        case None =>
          Future.failed(
            new IllegalArgumentException(
              s"Could not find ancestor for block=${tip.hashBE.hex}"))
      }

    }
  }

  /**
    * Calculate the next proof of work requirement for our blockchain
    * @see [[https://github.com/bitcoin/bitcoin/blob/35477e9e4e3f0f207ac6fa5764886b15bf9af8d0/src/pow.cpp#L49 bitcoin core implementation]]
    * @param currentTip
    * @param firstBlock
    * @param chainParams
    * @return
    */
  def calculateNextWorkRequired(
      currentTip: BlockHeaderDb,
      firstBlock: BlockHeaderDb,
      chainParams: ChainParams): Future[UInt32] = {
    if (chainParams.noRetargeting) {
      Future.successful(currentTip.nBits)
    } else {
      var actualTimespan = (currentTip.time - firstBlock.time).toLong
      val timespanSeconds = chainParams.powTargetTimeSpan.toSeconds
      if (actualTimespan < timespanSeconds / 4) {
        actualTimespan = timespanSeconds / 4
      }

      if (actualTimespan > timespanSeconds * 4) {
        actualTimespan = timespanSeconds * 4
      }

      val powLimit = chainParams.powLimit

      var bnNew = NumberUtil.targetExpansion(currentTip.nBits).difficulty

      bnNew = bnNew * actualTimespan

      bnNew = bnNew / timespanSeconds

      if (bnNew > powLimit) {
        bnNew = powLimit
      }

      val newTarget = NumberUtil.targetCompression(bnNew, false)

      Future.successful(newTarget)
    }
  }
}

object Pow extends Pow
