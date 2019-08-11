package org.bitcoins.chain.pow

import org.bitcoins.chain.blockchain.Blockchain
import org.bitcoins.chain.models.{BlockHeaderDb}
import org.bitcoins.core.number.UInt32
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.protocol.blockchain.{BlockHeader, ChainParams}
import org.bitcoins.core.util.NumberUtil

/**
  * Implements functions found inside of bitcoin core's
  * @see [[https://github.com/bitcoin/bitcoin/blob/35477e9e4e3f0f207ac6fa5764886b15bf9af8d0/src/pow.cpp pow.cpp]]
  */
sealed abstract class Pow {

  /**
    * Gets the next proof of work requirement for a block
    * @see [[https://github.com/bitcoin/bitcoin/blob/35477e9e4e3f0f207ac6fa5764886b15bf9af8d0/src/pow.cpp#L13 Mimics bitcoin core implmentation]]
    */
  def getNetworkWorkRequired(
      newPotentialTip: BlockHeader,
      blockchain: Blockchain)(implicit config: ChainAppConfig): UInt32 = {
    val chainParams = config.chain
    val tip = blockchain.tip
    val currentHeight = tip.height

    val powLimit: UInt32 =
      if ((currentHeight + 1) % chainParams.difficultyChangeInterval != 0) {
        if (chainParams.allowMinDifficultyBlocks) {
          // Special difficulty rule for testnet:
          // If the new block's timestamp is more than 2* 10 minutes
          // then allow mining of a min-difficulty block.
          if (newPotentialTip.time.toLong > tip.blockHeader.time.toLong + chainParams.powTargetSpacing.toSeconds * 2) {
            chainParams.compressedPowLimit
          } else {
            // Return the last non-special-min-difficulty-rules-block
            //while (pindex->pprev && pindex->nHeight % params.DifficultyAdjustmentInterval() != 0 && pindex->nBits == nProofOfWorkLimit)
            //                    pindex = pindex->pprev;
            val nonMinDiffF = blockchain.find { h =>
              h.nBits != chainParams.compressedPowLimit || h.height % chainParams.difficultyChangeInterval == 0
            }

            nonMinDiffF match {
              case Some(bh) => bh.nBits
              case None     =>
                //if we can't find a non min diffulty block, let's just fail
                throw new RuntimeException(
                  s"Could not find non mindiffulty block in chain! hash=${tip.hashBE.hex} height=${currentHeight}")
            }
          }
        } else {
          tip.blockHeader.nBits
        }
      } else {
        val firstHeight: Int = currentHeight - (chainParams.difficultyChangeInterval - 1)

        require(firstHeight >= 0,
                s"We must have our first height be postive, got=${firstHeight}")

        val firstBlockAtIntervalOpt: Option[BlockHeaderDb] =
          blockchain.findAtHeight(firstHeight)

        firstBlockAtIntervalOpt match {
          case Some(firstBlockAtInterval) =>
            calculateNextWorkRequired(currentTip = tip,
                                      firstBlockAtInterval,
                                      chainParams)
          case None =>
            throw new RuntimeException(
              s"Could not find block at height=${firstHeight} out of ${blockchain.length} headers to calculate pow difficutly change")
        }

      }

    powLimit
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
      chainParams: ChainParams): UInt32 = {
    if (chainParams.noRetargeting) {
      currentTip.nBits
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

      newTarget
    }
  }
}

object Pow extends Pow
