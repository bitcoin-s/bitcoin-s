package org.bitcoins.chain.validation

import org.bitcoins.chain.ChainVerificationLogger
import org.bitcoins.chain.pow.Pow
import org.bitcoins.core.api.chain.{
  Blockchain,
  ConnectTipResult,
  TipUpdateResult,
  TipValidationApi
}
import org.bitcoins.core.api.chain.db.{BlockHeaderDb, BlockHeaderDbHelper}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.blockchain.{BitcoinChainParams, BlockHeader}
import org.bitcoins.core.util.NumberUtil

/** Responsible for checking if we can connect two block headers together on the
  * blockchain. The checks things like proof of work difficulty, if it
  * references the previous block header correctly etc.
  */
sealed abstract class TipValidation
    extends TipValidationApi
    with ChainVerificationLogger {

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
      } else if (isBadMTP(newPotentialTip, blockchain)) {
        TipUpdateResult.BadMTP(newPotentialTip)
      } else if (isTimeTooNew(newPotentialTip)) {
        TipUpdateResult.TimeTooNew(newPotentialTip)
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

  override def connectTip(
      header: BlockHeader,
      blockchain: Blockchain,
      chainParams: BitcoinChainParams): ConnectTipResult = {
    logger.debug(
      s"Attempting to add new tip=${header.hashBE.hex} with prevhash=${header.previousBlockHashBE.hex} to chain"
    )

    val tipResult: ConnectTipResult = {
      findPrevBlockHeaderIdx(header, blockchain) match {
        case None =>
          logger.debug(
            s"No common ancestor found in the chain with tip=${blockchain.tip.hashBE.hex} to connect to hash=${header.hashBE.hex} prevHash=${header.previousBlockHashBE.hex}. This may be because we have a competing reorg!"
          )
          val err = TipUpdateResult.BadPreviousBlockHash(header)
          val failed = ConnectTipResult.BadTip(err)
          failed

        case Some(prevHeaderIdx) =>
          // found a header to connect to!
          val prevBlockHeaders = blockchain.headers.slice(
            prevHeaderIdx,
            prevHeaderIdx + chainParams.difficultyChangeInterval)
          val chain = Blockchain.fromHeaders(prevBlockHeaders)
          logger.debug(
            s"Attempting to add new tip=${header.hashBE.hex} with prevhash=${header.previousBlockHashBE.hex} to chain of ${blockchain.length} headers with tip ${blockchain.tip.hashBE.hex}"
          )
          val tipResult =
            TipValidation.checkNewTip(newPotentialTip = header,
                                      chain,
                                      chainParams)

          tipResult match {
            case success: TipUpdateResult.Success =>
              logger.debug(
                s"Successfully verified=${success.header.hashBE.hex}, connecting to chain"
              )
              val connectionIdx = blockchain.length - prevHeaderIdx

              // we construct a new blockchain by prepending the headers vector from the old one with the new tip
              // in order to avoid creating unnecessary hidden copies of the blockchain here
              if (connectionIdx != blockchain.length) {
                val newChain = Blockchain(
                  success.headerDb +: blockchain.headers.takeRight(
                    connectionIdx
                  )
                )
                // means we have a reorg, since we aren't connecting to latest tip
                ConnectTipResult.Reorg(success, newChain)
              } else {
                val olderChain = if (blockchain.size < 2016) {
                  blockchain.headers
                } else blockchain.headers.take(2015)
                val newChain = Blockchain(success.headerDb +: olderChain)
                // we just extended the latest tip
                ConnectTipResult.ExtendChain(success, newChain)
              }
            case fail: TipUpdateResult.Failure =>
              logger.warn(
                s"Could not verify header=${header.hashBE.hex}, reason=$fail"
              )
              ConnectTipResult.BadTip(fail)
          }
      }
    }
    tipResult
  }

  /** Finds the parent's index of the given header
    */
  private def findPrevBlockHeaderIdx(
      header: BlockHeader,
      blockchain: Blockchain
  ): Option[Int] = {
    // Let's see if we are lucky and the latest tip is the parent.
    val latestTip = blockchain.tip
    if (latestTip.hashBE == header.previousBlockHashBE) {
      // Yes we are.
      Some(0)
    } else {
      // No. Scanning the blockchain to find the parent.
      blockchain.findHeaderIdx(header.previousBlockHashBE)
    }
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
      headerWork > header.target
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

  private def isBadMTP(
      newPotentialTip: BlockHeader,
      blockchain: Blockchain
  ): Boolean = {
    blockchain.getMedianTimePast match {
      case None =>
        // if median time past cannot be computed, do not silently
        // skip MTP validation.
        false
      case Some(medianTimePast) =>
        newPotentialTip.time.toLong <= medianTimePast
    }
  }

  private def isTimeTooNew(header: BlockHeader): Boolean = {
    val currentTime = System.currentTimeMillis() / 1000
    header.time.toLong > currentTime + 2 * 60 * 60
  }
}

object TipValidation extends TipValidation
