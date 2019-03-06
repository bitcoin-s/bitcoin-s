package org.bitcoins.chain.validation

import org.bitcoins.chain.models.{BlockHeaderDb, BlockHeaderDbHelper}
import org.bitcoins.core.protocol.blockchain.{BlockHeader, ChainParams}
import org.bitcoins.core.util.{BitcoinSLogger, NumberUtil}

sealed abstract class TipValidation extends BitcoinSLogger {

  /** Checks if the given header can be connected to the current tip
    * This is the method where a [[BlockHeader]] is transformed into a
    * [[BlockHeaderDb]]. What this really means is that a height is
    * assigned to a [[BlockHeader header]] after all these
    * validation checks occur
    * */
  def checkNewTip(
      newPotentialTip: BlockHeader,
      currentTip: BlockHeaderDb,
      chainParams: ChainParams): TipUpdateResult = {
    val header = newPotentialTip
    logger.info(
      s"Checking header=${header.hashBE.hex} to try to connect to currentTip=${currentTip.hashBE.hex} with height=${currentTip.height}")

    val connectTipResult = {
      if (header.previousBlockHashBE != currentTip.hashBE) {
        logger.warn(
          s"Failed to connect tip=${header.hashBE.hex} to current chain")
        TipUpdateResult.BadPreviousBlockHash(newPotentialTip)
      } else if ((header.nBits != currentTip.nBits && !checkPOWInterval(
                   currentTip = currentTip,
                   chainParams = chainParams))) {
        //https://github.com/bitcoin/bitcoin/blob/eb7daf4d600eeb631427c018a984a77a34aca66e/src/pow.cpp#L19
        TipUpdateResult.BadPOW(newPotentialTip)
      } else if (isBadNonce(newPotentialTip)) {
        TipUpdateResult.BadNonce(newPotentialTip)
      } else {
        val headerDb = BlockHeaderDbHelper.fromBlockHeader(
          height = currentTip.height + 1,
          bh = newPotentialTip
        )
        TipUpdateResult.Success(headerDb)
      }
    }

    logTipResult(connectTipResult, currentTip)

    connectTipResult
  }

  /** Returns true if this is a valid proof of work change, else false */
  private def checkPOWInterval(
      currentTip: BlockHeaderDb,
      chainParams: ChainParams): Boolean = {
    val newPotentialHeight = currentTip.height + 1
    val consensusPowInteval = chainParams.difficultyChangeInterval

    newPotentialHeight % consensusPowInteval == 0
  }

  /** Logs the result of [[org.bitcoins.chain.validation.TipValidation.checkNewTip() checkNewTip]] */
  private def logTipResult(
      connectTipResult: TipUpdateResult,
      currentTip: BlockHeaderDb): Unit = {
    connectTipResult match {
      case TipUpdateResult.Success(tipDb) =>
        logger.info(
          s"Successfully connected ${tipDb.hashBE.hex} with height=${tipDb.height} to block=${currentTip.hashBE.hex} with height=${currentTip.height}")

      case bad: TipUpdateResult.Failure =>
        logger.warn(
          s"Failed to connect ${bad.header.hashBE.hex} to ${currentTip.hashBE.hex} with height=${currentTip.height}, reason=${bad}")
    }
  }

  /** Checks if [[header.nonce]] hashes to meet the POW requirements for this block (nBits)
    * Mimics this
    * [[https://github.com/bitcoin/bitcoin/blob/eb7daf4d600eeb631427c018a984a77a34aca66e/src/pow.cpp#L74]]
    * */
  private def isBadNonce(header: BlockHeader): Boolean = {
    //convert hash into a big integer
    val headerWork = BigInt(1, header.hashBE.bytes.toArray)
    if (headerWork <= 0 || NumberUtil.isNBitsOverflow(nBits = header.nBits)) {
      true
    } else {
      headerWork > header.difficulty
    }

  }
}

object TipValidation extends TipValidation
