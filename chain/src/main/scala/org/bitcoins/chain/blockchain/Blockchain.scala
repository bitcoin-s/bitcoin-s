package org.bitcoins.chain.blockchain

import org.bitcoins.chain.models.{BlockHeaderDAO, BlockHeaderDb}
import org.bitcoins.chain.validation.{TipUpdateResult, TipValidation}
import org.bitcoins.core.protocol.blockchain.{BlockHeader, ChainParams}
import org.bitcoins.core.util.BitcoinSLogger

import scala.concurrent.{ExecutionContext, Future}

/**
  * In memory implementation of a blockchain
  * This data structure maintains the state of a
  * blockchain in memory, the headers can be accessed
  * with [[headers]]. The headers are stored with the most
  * recent header at index 0, the second most recent header at index 1 etc
  * You can walk the chain by
  * {{{
  *   headers.map(h => println(h))
  * }}}
  *
  */
case class Blockchain(
    headers: Vector[BlockHeaderDb],
    blockHeaderDAO: BlockHeaderDAO,
    chainParams: ChainParams)
    extends BitcoinSLogger {
  //TODO: Think about not exposing the headers to world, encapsulate them and
  //provide methods like `.map` and `.foreach` on this data structure.

  def tip: BlockHeaderDb = headers.head

  def connectTip(header: BlockHeader)(
      implicit ec: ExecutionContext): Future[BlockchainUpdate] = {
    logger.debug(
      s"Attempting to add new tip=${header.hashBE.hex} to chain with current tip=${tip.hashBE.hex}")
    val tipResultF =
      TipValidation.checkNewTip(newPotentialTip = header,
                                currentTip = tip,
                                blockHeaderDAO = blockHeaderDAO,
                                chainParams = chainParams)

    tipResultF.map { tipResult =>
      tipResult match {
        case TipUpdateResult.Success(headerDb) =>
          val newChain = Blockchain.fromHeaders(headerDb +: headers,
                                                blockHeaderDAO,
                                                chainParams)
          BlockchainUpdate.Successful(newChain, headerDb)
        case fail: TipUpdateResult.Failure =>
          BlockchainUpdate.Failed(this, header, fail)
      }
    }
  }
}

object Blockchain {

  def fromHeaders(
      headers: Vector[BlockHeaderDb],
      blockHeaderDAO: BlockHeaderDAO,
      chainParams: ChainParams): Blockchain = {
    Blockchain(headers, blockHeaderDAO, chainParams)
  }
}
