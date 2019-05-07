package org.bitcoins.chain.validation

import org.bitcoins.chain.models.{BlockHeaderDAO, BlockHeaderDb, BlockHeaderDbHelper}
import org.bitcoins.chain.pow.Pow
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.core.util.{BitcoinSLogger, NumberUtil}

import scala.concurrent.{ExecutionContext, Future}

/**
  * Responsible for checking if we can connect two
  * block headers together on the blockchain. The checks
  * things like proof of work difficulty, if it
  * references the previous block header correctly etc.
  */
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
      blockHeaderDAO: BlockHeaderDAO)(
      implicit ec: ExecutionContext): Future[TipUpdateResult] = {
    val header = newPotentialTip
    logger.info(
      s"Checking header=${header.hashBE.hex} to try to connect to currentTip=${currentTip.hashBE.hex} with height=${currentTip.height}")

    val powCheckF = isBadPow(newPotentialTip = newPotentialTip,
                             currentTip = currentTip,
                             blockHeaderDAO = blockHeaderDAO)

    val connectTipResultF: Future[TipUpdateResult] = {
      powCheckF.map { expectedWork =>
        if (header.previousBlockHashBE != currentTip.hashBE) {
          logger.warn(
            s"Failed to connect tip=${header.hashBE.hex} to current chain")
          TipUpdateResult.BadPreviousBlockHash(newPotentialTip)
        } else if (header.nBits != expectedWork) {
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
    }

    logTipResult(connectTipResultF, currentTip)
    connectTipResultF
  }

  /** Logs the result of [[org.bitcoins.chain.validation.TipValidation.checkNewTip() checkNewTip]] */
  private def logTipResult(
      connectTipResultF: Future[TipUpdateResult],
      currentTip: BlockHeaderDb)(implicit ec: ExecutionContext): Unit = {
    connectTipResultF.map {
      case TipUpdateResult.Success(tipDb) =>
        logger.info(
          s"Successfully connected ${tipDb.hashBE.hex} with height=${tipDb.height} to block=${currentTip.hashBE.hex} with height=${currentTip.height}")

      case bad: TipUpdateResult.Failure =>
        logger.warn(
          s"Failed to connect ${bad.header.hashBE.hex} to ${currentTip.hashBE.hex} with height=${currentTip.height}, reason=${bad}")

    }

    ()
  }

  /** Checks if [[header]] hashes to meet the POW requirements for this block (nBits)
    * Mimics this
    * @see [[https://github.com/bitcoin/bitcoin/blob/eb7daf4d600eeb631427c018a984a77a34aca66e/src/pow.cpp#L74]]
    * */
  def isBadNonce(header: BlockHeader): Boolean = {
    //convert hash into a big integer
    val headerWork = BigInt(1, header.hashBE.bytes.toArray)
    if (headerWork <= 0 || NumberUtil.isNBitsOverflow(nBits = header.nBits)) {
      true
    } else {
      headerWork > header.difficulty
    }
  }

  private def isBadPow(
      newPotentialTip: BlockHeader,
      currentTip: BlockHeaderDb,
      blockHeaderDAO: BlockHeaderDAO)(
      implicit ec: ExecutionContext): Future[UInt32] = {
    Pow.getNetworkWorkRequired(tip = currentTip,
                               newPotentialTip = newPotentialTip,
                               blockHeaderDAO = blockHeaderDAO)

  }
}

object TipValidation extends TipValidation
