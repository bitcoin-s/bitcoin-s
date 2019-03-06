package org.bitcoins.chain.blockchain

import org.bitcoins.chain.models.BlockHeaderDb
import org.bitcoins.chain.validation.{TipUpdateResult, TipValidation}
import org.bitcoins.core.protocol.blockchain.{BlockHeader, ChainParams}
import org.bitcoins.core.util.BitcoinSLogger

import scala.annotation.tailrec

sealed abstract class Blockchain extends BitcoinSLogger {
  require(
    connectionInvariant,
    s"Our blockchain is not connect correctly, this means a header does not refer to a previous block header correctly")
  def tip: BlockHeaderDb = headers.head

  def chainParams: ChainParams

  def headers: Vector[BlockHeaderDb]

  def connectTip(header: BlockHeader): BlockchainUpdate = {
    logger.debug(
      s"Attempting to add new tip=${header.hashBE.hex} to chain with current tip=${tip.hashBE.hex}")
    val tipResult =
      TipValidation.checkNewTip(header, currentTip = tip, chainParams)

    tipResult match {
      case TipUpdateResult.Success(headerDb) =>
        val newChain = Blockchain.fromHeaders(headerDb +: headers, chainParams)
        BlockchainUpdate.Successful(newChain, headerDb)
      case fail: TipUpdateResult.Failure =>
        BlockchainUpdate.Failed(this, header, fail)
    }
  }

  /** Helper method to ensure our blockchain is properly connected at all times */
  private def connectionInvariant: Boolean = {
    @tailrec
    def loop(remaining: List[BlockHeaderDb]): Boolean = {
      remaining match {
        case tip :: oldTip :: t =>
          val tipValidation =
            TipValidation.checkNewTip(tip.blockHeader, oldTip, chainParams)
          tipValidation match {
            case TipUpdateResult.Success(_) =>
              loop(t)
            case fail: TipUpdateResult.Failure =>
              logger.error(
                s"Failed connection invariant for blockchain, reason=${fail}")
              false
          }
        case _ :: Nil | Nil => true
      }
    }

    loop(headers.toList)
  }
}

object Blockchain {
  private case class BlockchainImpl(
      headers: Vector[BlockHeaderDb],
      chainParams: ChainParams)
      extends Blockchain

  def fromHeaders(
      headers: Vector[BlockHeaderDb],
      chainParams: ChainParams): Blockchain = {
    BlockchainImpl(headers, chainParams)
  }
}
