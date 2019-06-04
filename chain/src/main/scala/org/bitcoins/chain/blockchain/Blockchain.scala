package org.bitcoins.chain.blockchain

import org.bitcoins.chain.models.{BlockHeaderDAO, BlockHeaderDb}
import org.bitcoins.chain.validation.{TipUpdateResult, TipValidation}
import org.bitcoins.core.protocol.blockchain.BlockHeader
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
case class Blockchain(headers: Vector[BlockHeaderDb]) extends BitcoinSLogger {
  val tip: BlockHeaderDb = headers.head
}

object Blockchain extends BitcoinSLogger {

  def fromHeaders(headers: Vector[BlockHeaderDb]): Blockchain = {
    Blockchain(headers)
  }

  /**
    * Attempts to connect the given block header with the given blockchain
    * This is done via the companion object for blockchain because
    * we query [[BlockHeaderDAO block header dao]] for the chain tips
    * We then attempt to connect this block header to all of our current
    * chain tips.
    * @param header the block header to connect to our chain
    * @param blockHeaderDAO where we can find our blockchain
    * @param ec
    * @return a [[Future future]] that contains a [[BlockchainUpdate update]] indicating
    *         we [[BlockchainUpdate.Successful successfully]] connected the tip,
    *         or [[BlockchainUpdate.Failed failed]] to connect to a tip
    */
  def connectTip(header: BlockHeader, blockHeaderDAO: BlockHeaderDAO)(
      implicit ec: ExecutionContext): Future[BlockchainUpdate] = {

    //get all competing chains we have
    val blockchainsF: Future[Vector[Blockchain]] =
      blockHeaderDAO.getBlockchains()

    val tipResultF: Future[BlockchainUpdate] = blockchainsF.flatMap {
      blockchains =>
        val nested: Vector[Future[BlockchainUpdate]] = blockchains.map {
          blockchain =>
            val tip = blockchain.tip
            logger.info(
              s"Attempting to add new tip=${header.hashBE.hex} with prevhash=${header.previousBlockHashBE.hex} to chain with current tips=${tip.hashBE.hex}")
            val tipResultF = TipValidation.checkNewTip(newPotentialTip = header,
                                                       currentTip = tip,
                                                       blockHeaderDAO =
                                                         blockHeaderDAO)

            tipResultF.map { tipResult =>
              tipResult match {
                case TipUpdateResult.Success(headerDb) =>
                  val newChain =
                    Blockchain.fromHeaders(headerDb +: blockchain.headers)
                  BlockchainUpdate.Successful(newChain, headerDb)
                case fail: TipUpdateResult.Failure =>
                  BlockchainUpdate.Failed(blockchain, header, fail)
              }
            }
        }
        parseSuccessOrFailure(nested = nested)
    }

    tipResultF
  }

  /** Takes in a vector of blockchain updates being executed asynchronously, we can only connect one [[BlockHeader header]]
    * to a tip successfully, which means _all_ other [[BlockchainUpdate updates]] must fail. This is a helper method
    * to find the one [[BlockchainUpdate.Successful successful]] update, or else returns one of the [[BlockchainUpdate.Failed failures]]
    * @param nested
    * @param ec
    * @return
    */
  private def parseSuccessOrFailure(nested: Vector[Future[BlockchainUpdate]])(
      implicit ec: ExecutionContext): Future[BlockchainUpdate] = {
    val successfulTipOptF: Future[Option[BlockchainUpdate]] = {
      Future.find(nested) {
        case update: BlockchainUpdate =>
          update.isInstanceOf[BlockchainUpdate.Successful]
      }
    }

    successfulTipOptF.flatMap {
      case Some(update) => Future.successful(update)
      case None         =>
        //if we didn't successfully connect a tip, just take the first failure we see
        Future
          .find(nested) {
            case update: BlockchainUpdate =>
              update.isInstanceOf[BlockchainUpdate.Failed]
          }
          .map(_.get)
    }
  }
}
