package org.bitcoins.chain.validation

import org.bitcoins.chain.ChainVerificationLogger
import org.bitcoins.chain.blockchain.Blockchain
import org.bitcoins.chain.pow.Pow
import org.bitcoins.core.api.chain.db.{BlockHeaderDb, BlockHeaderDbHelper}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.blockchain.{BitcoinChainParams, BlockHeader}
import org.bitcoins.core.util.NumberUtil

/** Responsible for checking if we can connect two block headers together on the
  * blockchain. The checks things like proof of work difficulty, if it
  * references the previous block header correctly etc.
  */
sealed abstract class TipValidation extends ChainVerificationLogger {

  /** Checks if the given header can be connected to the current tip This is the
    * method where a
    * [[org.bitcoins.core.protocol.blockchain.BlockHeader BlockHeader]] is
    * transformed into a
    * [[org.bitcoins.chain.models.BlockHeaderDb BlockHeaderDb]]. What this
    * really means is that a height is assigned to a
    * [[org.bitcoins.core.protocol.blockchain.BlockHeader BlockHeader]] after
    * all these validation checks occur
    */
  def checkNewTip(
      newPotentialTip: BlockHeader,
      blockchain: Blockchain,
      chainParams: BitcoinChainParams): TipUpdateResult = {
    val header = newPotentialTip
    val currentTip = blockchain.tip
    logger.trace(
      s"Checking header=${header.hashBE.hex} to try to connect to currentTip=${currentTip.hashBE.hex} with height=${currentTip.height}"
    )

    val expectedWork: UInt32 =
      isBadPow(newPotentialTip = newPotentialTip,
               blockchain = blockchain,
               chainParams = chainParams)

    val connectTipResult: TipUpdateResult = {
      if (header.previousBlockHashBE != currentTip.hashBE) {
        logger.warn(
          s"Failed to connect tip=${header.hashBE.hex} to current chain"
        )
        TipUpdateResult.BadPreviousBlockHash(newPotentialTip)
      } else if (header.nBits != expectedWork) {
        // https://github.com/bitcoin/bitcoin/blob/eb7daf4d600eeb631427c018a984a77a34aca66e/src/pow.cpp#L19
        TipUpdateResult.BadPOW(newPotentialTip)
      } else if (isBadNonce(newPotentialTip)) {
        TipUpdateResult.BadNonce(newPotentialTip)
      } else {
        val headerDb = BlockHeaderDbHelper.fromBlockHeader(
          height = currentTip.height + 1,
          chainWork = currentTip.chainWork + Pow.getBlockProof(newPotentialTip),
          bh = newPotentialTip
        )
        TipUpdateResult.Success(headerDb)
      }
    }

    logTipResult(connectTipResult, currentTip)
    connectTipResult
  }

  /** Logs the result of
    * [[org.bitcoins.chain.validation.TipValidation.checkNewTip() checkNewTip]]
    */
  private def logTipResult(
      connectTipResult: TipUpdateResult,
      currentTip: BlockHeaderDb
  ): Unit = {
    connectTipResult match {
      case TipUpdateResult.Success(tipDb) =>
        logger.trace(
          s"Successfully connected ${tipDb.hashBE.hex} with height=${tipDb.height} to block=${currentTip.hashBE.hex} with height=${currentTip.height}"
        )

      case bad: TipUpdateResult.Failure =>
        logger.warn(
          s"Failed to connect ${bad.header.hashBE.hex} to ${currentTip.hashBE.hex} with height=${currentTip.height}, reason=${bad}"
        )

    }
    ()
  }

  /** Checks if the given header hashes to meet the POW requirements for this
    * block (determined by lookinng at the `nBits` field).
    *
    * @see
    *   [[https://github.com/bitcoin/bitcoin/blob/eb7daf4d600eeb631427c018a984a77a34aca66e/src/pow.cpp#L74 pow.cpp]]
    *   in Bitcoin Core
    */
  def isBadNonce(header: BlockHeader): Boolean = {
    // convert hash into a big integer
    val headerWork = BigInt(1, header.hashBE.bytes.toArray)
    if (headerWork <= 0 || NumberUtil.isNBitsOverflow(nBits = header.nBits)) {
      true
    } else {
      headerWork > header.difficulty
    }
  }

  private def isBadPow(
      newPotentialTip: BlockHeader,
      blockchain: Blockchain,
      chainParams: BitcoinChainParams): UInt32 = {
    Pow.getNetworkWorkRequired(
      newPotentialTip = newPotentialTip,
      blockchain = blockchain,
      chainParams = chainParams
    )

  }
}

object TipValidation extends TipValidation
