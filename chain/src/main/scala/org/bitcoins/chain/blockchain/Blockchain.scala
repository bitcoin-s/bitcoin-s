package org.bitcoins.chain.blockchain

import org.bitcoins.chain.models.BlockHeaderDb
import org.bitcoins.chain.validation.{TipUpdateResult, TipValidation}
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.core.util.BitcoinSLogger

import scala.annotation.tailrec

sealed abstract class Blockchain extends BitcoinSLogger {
  require(
    connectionInvariant,
    s"Our blockchain is not connect correctly, this means a header does not refer to a previous block header correctly")
  def tip: BlockHeaderDb = headers.head

  def headers: Vector[BlockHeaderDb]

  def connectTip(header: BlockHeader): BlockchainUpdate = {
    logger.debug(
      s"Attempting to add new tip=${header.hashBE.hex} to chain with current tip=${tip.hashBE.hex}")
    val tipResult =
      TipValidation.checkNewTip(header, currentTip = tip)

    tipResult match {
      case TipUpdateResult.Success(headerDb) =>
        val newChain = Blockchain.fromHeaders(headerDb +: headers)
        BlockchainUpdate.Successful(newChain, headerDb)
      case fail: TipUpdateResult.TipUpdateFailure =>
        BlockchainUpdate.Failed(this, header, fail)
    }
  }

  /** Helper method to ensure our blockchain is properly connected at all times */
  private def connectionInvariant: Boolean = {
    val noTip = headers.tail
    @tailrec
    def loop(
        remaining: List[BlockHeaderDb],
        nextHeader: BlockHeaderDb): Boolean = {
      remaining match {
        case tip :: oldTip :: t =>
          val tipValidation =
            TipValidation.checkNewTip(tip.blockHeader, oldTip)
          tipValidation match {
            case TipUpdateResult.Success(_) =>
              loop(t, oldTip)
            case fail: TipUpdateResult.TipUpdateFailure =>
              logger.error(
                s"Failed connection invariant for blockchain, reason=${fail}")
              false
          }
        case _ :: Nil | Nil => true
      }
    }

    loop(noTip.toList, tip)
  }
}

object Blockchain {
  private case class BlockchainImpl(headers: Vector[BlockHeaderDb])
      extends Blockchain

  def fromHeaders(headers: Vector[BlockHeaderDb]): Blockchain = {
    BlockchainImpl(headers)
  }
}
