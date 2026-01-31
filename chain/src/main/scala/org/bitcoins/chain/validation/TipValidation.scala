package org.bitcoins.chain.validation

import org.bitcoins.chain.ChainVerificationLogger
import org.bitcoins.chain.blockchain.Blockchain
import org.bitcoins.chain.pow.Pow
import org.bitcoins.core.api.chain.db.{BlockHeaderDb, BlockHeaderDbHelper}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.blockchain.{
  BitcoinChainParams,
  Block,
  BlockHeader,
  MainNetChainParams,
  RegTestNetChainParams,
  SigNetChainParams,
  TestNet4ChainParams,
  TestNetChainParams
}
import org.bitcoins.core.protocol.transaction.{
  Transaction,
  TransactionConstants
}
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

  def contextualCheckBlock(block: Block, blockchain: Blockchain): Boolean = {
    val tip = blockchain.tip
    require(
      block.blockHeader.previousBlockHashBE == tip.hashBE,
      s"Block ${block.blockHeader.hashBE} previous hash ${block.blockHeader.previousBlockHashBE.hex} did not match tip hash ${tip.hashBE.hex}"
    )
    val nHeight = tip.height + 1
    val locktimeCutOff = blockchain.getMedianTimePast
    val allTxsFinal =
      block.transactions.forall(isFinalTx(_, nHeight, locktimeCutOff))
    val cbTx = block.transactions.head
    val cbInput = cbTx.inputs.head
    val cbLockTime = cbTx.lockTime
    val cbHeight = cbInput.scriptSignature.asm.head.toLong
    val cbSequence = cbInput.sequence
    if (!allTxsFinal) {
      false
    } else if (cbLockTime.toLong != (nHeight - 1)) {
      false
    } else if (cbHeight != nHeight) {
      false
    } else if (cbSequence == UInt32.max) {
      false
    } else {
      true
    }
  }

  /** See
    * [[https://github.com/bitcoin/bitcoin/blob/ab233255d444ccf6ffe4a45cb02bfc3e5fb71bdb/src/validation.cpp#L4147]]
    */
  def contextualCheckBlockHeader(
      header: BlockHeader,
      blockchain: Blockchain,
      chainParams: BitcoinChainParams): Boolean = {
    val tip = blockchain.tip
    require(
      header.previousBlockHashBE == tip.hashBE,
      s"BlockHeader ${header.hashBE} previous hash ${header.previousBlockHashBE.hex} did not match tip hash ${tip.hashBE.hex}"
    )

    // Defines how many seconds earlier the timestamp of the first block in a difficulty adjustment
    // period can be compared to the last block of the previous period (BIP54).
    val maxTimeWarpOpt: Option[Int] = chainParams match {
      case MainNetChainParams  => Some(600 * 12)
      case TestNet4ChainParams => Some(60 * 12)
      case TestNetChainParams | SigNetChainParams(_) | RegTestNetChainParams =>
        None
    }

    if (
      header.nBits != isBadPow(header, blockchain, chainParams = chainParams)
    ) {
      false
    } else if (header.time.toLong <= blockchain.getMedianTimePast) {
      false
    } else if (checkTimeWarp(maxTimeWarpOpt, header, tip, chainParams)) {
      false
    } else if (checkMurchZawyAttack(header, tip, blockchain, chainParams)) {
      false
    } else {
      true
    }
  }

  /** see
    * [[https://github.com/darosior/bitcoin/blob/f24256fbe610fd61bbede8376a1fe3d34f29bec5/src/validation.cpp#L4236]]
    */
  private def checkMurchZawyAttack(
      header: BlockHeader,
      tip: BlockHeaderDb,
      blockchain: Blockchain,
      params: BitcoinChainParams): Boolean = {
    val nHeight = tip.height + 1
    val diffInterval = params.difficultyChangeInterval
    if (nHeight % diffInterval == diffInterval - 1) {
      val startHeight = nHeight - diffInterval + 1
      val startBlockOpt = blockchain.findAtHeight(startHeight)
      require(startBlockOpt.isDefined,
              s"Could not find block at height=$startHeight")
      header.time < startBlockOpt.get.blockHeader.time
    } else {
      false
    }
  }

  /** See
    * [[https://github.com/darosior/bitcoin/blob/f24256fbe610fd61bbede8376a1fe3d34f29bec5/src/validation.cpp#L4226]]
    */
  private def checkTimeWarp(
      maxTimeWarpOpt: Option[Int],
      header: BlockHeader,
      tip: BlockHeaderDb,
      chainParams: BitcoinChainParams): Boolean = {
    val newHeight = tip.height + 1
    maxTimeWarpOpt.isDefined && (newHeight % chainParams.difficultyChangeInterval == 0) &&
    (header.time.toLong < (tip.blockHeader.time.toLong - maxTimeWarpOpt.get))
  }

  def isFinalTx(tx: Transaction, blockHeight: Int, blockTime: Long): Boolean = {
    val t = TransactionConstants
    val isBlockHeight = tx.lockTime < t.locktimeThreshold
    if (tx.lockTime == UInt32.zero) {
      return true
    }
    if (isBlockHeight && tx.lockTime.toLong < blockHeight) {
      return true
    }
    if (!isBlockHeight && tx.lockTime.toLong < blockTime) {
      return true
    }
    tx.inputs.forall(_.sequence == UInt32.max)
  }
}

object TipValidation extends TipValidation
