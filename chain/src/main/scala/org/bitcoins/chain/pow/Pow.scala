package org.bitcoins.chain.pow

import org.bitcoins.chain.blockchain.Blockchain
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.BlockHeaderDb
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.blockchain._
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
              case None =>
                config.chain match {
                  case RegTestNetChainParams =>
                    RegTestNetChainParams.compressedPowLimit
                  case TestNetChainParams | MainNetChainParams =>
                    //if we can't find a non min diffulty block, let's just fail
                    throw new RuntimeException(
                      s"Could not find non mindifficulty block in chain of size=${blockchain.length}! hash=${tip.hashBE.hex} height=${currentHeight}")
                }

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

  def getBlockProof(header: BlockHeader): UInt32 = {
    // taken from: https://github.com/bitcoin/bitcoin/blob/aa8d76806c74a51ec66e5004394fe9ea8ff0fac4/src/chain.cpp#L122

    // We need to compute 2**256 / (bnTarget+1), but we can't represent 2**256
    // as it's too large for an UInt32. However, as 2**256 is at least as large
    // as bnTarget+1, it is equal to ((2**256 - bnTarget - 1) / (bnTarget+1)) + 1,
    // or ~bnTarget / (bnTarget+1) + 1.
    val bnTarget = header.nBits
    val inverseBytes = bnTarget.bytes.map(byte => (byte ^ 0xff).toByte)
    val inverse = UInt32(inverseBytes)

    (inverse / (bnTarget + UInt32.one)) + UInt32.one
  }
}

object Pow extends Pow
