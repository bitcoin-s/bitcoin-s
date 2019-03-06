package org.bitcoins.chain.validation

import org.bitcoins.chain.blockchain.Blockchain
import org.bitcoins.chain.models.{BlockHeaderDb, BlockHeaderDbHelper}
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.core.util.BitcoinSLogger

sealed abstract class TipValidation extends BitcoinSLogger {

  /** Checks if the given header can be connected to the current tip
    * This is the method where a [[BlockHeader]] is transformed into a
    * [[BlockHeaderDb]]. What this really means is that a height is
    * assigned to a [[BlockHeader header]] after all these
    * validation checks occur
    * */
  def checkNewTip(
      newPotentialTip: BlockHeader,
      currentTip: BlockHeaderDb): TipUpdateResult = {
    val header = newPotentialTip
    logger.debug(
      s"Checking header=${header.hashBE.hex} to try to connect to currentTip=${currentTip.hashBE.hex}")
    if (header.previousBlockHashBE != currentTip.hashBE) {
      logger.warn(
        s"Failed to connect tip=${header.hashBE.hex} to current chain")
      TipUpdateResult.BadPreviousBlockHash(newPotentialTip)
    } else if (header.nBits != currentTip.nBits) {
      //TODO: THis is a bug, this should only occurr only 2016 blocks
      //also this doesn't apply on testnet/regtest
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

  private def isBadNonce(header: BlockHeader): Boolean = {
    //TODO: implment me
    false
  }
}

object TipValidation extends TipValidation
