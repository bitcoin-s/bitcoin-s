package org.bitcoins.chain.blockchain

import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.BlockHeaderDb
import org.bitcoins.chain.validation.{TipUpdateResult, TipValidation}
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.chain.ChainVerificationLogger

import scala.annotation.tailrec
import scala.collection.{IndexedSeqLike, mutable}

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
case class Blockchain(headers: Vector[BlockHeaderDb])
    extends IndexedSeqLike[BlockHeaderDb, Vector[BlockHeaderDb]] {
  val tip: BlockHeaderDb = headers.head

  /** @inheritdoc */
  override def newBuilder: mutable.Builder[
    BlockHeaderDb,
    Vector[BlockHeaderDb]] = Vector.newBuilder[BlockHeaderDb]

  /** @inheritdoc */
  override def seq: IndexedSeq[BlockHeaderDb] = headers

  /** @inheritdoc */
  override def length: Int = headers.length

  /** @inheritdoc */
  override def apply(idx: Int): BlockHeaderDb = headers.apply(idx)

  /** Finds a block header at a given height */
  def findAtHeight(height: Int): Option[BlockHeaderDb] =
    find(_.height == height)

  /** Splits the blockchain at the header, returning a new blockchain where the best tip is the given header */
  def fromHeader(header: BlockHeaderDb): Option[Blockchain] = {
    val headerIdxOpt = headers.zipWithIndex.find(_._1 == header)
    headerIdxOpt.map {
      case (header, idx) =>
        val newChain = Blockchain.fromHeaders(headers.splitAt(idx)._2)
        require(newChain.tip == header)
        newChain
    }
  }

  /** The height of the chain */
  def height: Int = tip.height

}

object Blockchain extends ChainVerificationLogger {

  def fromHeaders(headers: Vector[BlockHeaderDb]): Blockchain = {

    Blockchain(headers)
  }

  /**
    * Attempts to connect the given block header with the given blockchain
    * This is done via the companion object for blockchain because
    * we query [[org.bitcoins.chain.models.BlockHeaderDAO BlockHeaderDAO]] for the chain tips
    * We then attempt to connect this block header to all of our current
    * chain tips.
    * @param header the block header to connect to our chain
    * @param blockchain the blockchain we are attempting to connect to
    * @return a [[scala.concurrent.Future Future]] that contains a [[org.bitcoins.chain.blockchain.BlockchainUpdate BlockchainUpdate]] indicating
    *         we [[org.bitcoins.chain.blockchain.BlockchainUpdate.Successful successful]] connected the tip,
    *         or [[org.bitcoins.chain.blockchain.BlockchainUpdate.Failed Failed]] to connect to a tip
    */
  def connectTip(header: BlockHeader, blockchain: Blockchain)(
      implicit conf: ChainAppConfig): BlockchainUpdate = {
    logger.debug(
      s"Attempting to add new tip=${header.hashBE.hex} with prevhash=${header.previousBlockHashBE.hex} to chain")

    val tipResult: BlockchainUpdate = {
      val prevBlockHeaderIdxOpt =
        blockchain.headers.zipWithIndex.find {
          case (headerDb, _) =>
            headerDb.hashBE == header.previousBlockHashBE
        }
      prevBlockHeaderIdxOpt match {
        case None =>
          logger.warn(
            s"No common ancestor found in the chain to connect to ${header.hashBE}")
          val err = TipUpdateResult.BadPreviousBlockHash(header)
          val failed = BlockchainUpdate.Failed(blockchain = blockchain,
                                               successfulHeaders = Vector.empty,
                                               failedHeader = header,
                                               tipUpdateFailure = err)
          failed

        case Some((prevBlockHeader, prevHeaderIdx)) =>
          //found a header to connect to!
          logger.debug(
            s"Attempting to add new tip=${header.hashBE.hex} with prevhash=${header.previousBlockHashBE.hex} to chain")
          val chain = blockchain.fromHeader(prevBlockHeader)
          val tipResult =
            TipValidation.checkNewTip(newPotentialTip = header, chain.get)

          tipResult match {
            case TipUpdateResult.Success(headerDb) =>
              logger.debug(
                s"Successfully verified=${headerDb.hashBE.hex}, connecting to chain")
              val oldChain =
                blockchain.takeRight(blockchain.length - prevHeaderIdx)
              val newChain =
                Blockchain.fromHeaders(headerDb +: oldChain)
              BlockchainUpdate.Successful(newChain, Vector(headerDb))
            case fail: TipUpdateResult.Failure =>
              logger.warn(
                s"Could not verify header=${header.hashBE.hex}, reason=$fail")
              BlockchainUpdate.Failed(blockchain = blockchain,
                                      successfulHeaders = Vector.empty,
                                      failedHeader = header,
                                      tipUpdateFailure = fail)
          }
      }
    }
    tipResult
  }

  /** Iterates through each given blockchains attempting to connect the given headers to that chain
    * @return The final updates for each chain
    *
    * */
  def connectHeadersToChains(
      headers: Vector[BlockHeader],
      blockchains: Vector[Blockchain])(
      implicit chainAppConfig: ChainAppConfig): Vector[BlockchainUpdate] = {
    @tailrec
    def loop(
        headersToProcess: List[BlockHeader],
        lastUpdate: BlockchainUpdate): BlockchainUpdate = {
      lastUpdate match {
        case BlockchainUpdate.Successful(blockchain, updatedHeaders) =>
          headersToProcess match {
            case h :: t =>
              val newUpdate =
                Blockchain.connectTip(header = h, blockchain = blockchain)
              val newHeaders = blockchain.tip +: lastUpdate.successfulHeaders

              loop(headersToProcess = t,
                   lastUpdate = newUpdate.addSuccessfulHeader())
            case Nil =>
              BlockchainUpdate.Successful(
                blockchain,
                successfulHeaders = updatedHeaders
              )
          }
        case f: BlockchainUpdate.Failed => f
      }
    }

    blockchains.map { blockchain =>
      val initUpdate = BlockchainUpdate.Successful(blockchain, Vector.empty)
      loop(headersToProcess = headers.toList, lastUpdate = initUpdate)
    }
  }

  /** Takes in a vector of blockchain updates being executed asynchronously, we can only connect one [[BlockHeader header]]
    * to a tip successfully, which means _all_ other [[BlockchainUpdate updates]] must fail. This is a helper method
    * to find the one [[BlockchainUpdate.Successful successful]] update, or else returns one of the [[BlockchainUpdate.Failed failures]]
    * @return
    */
  private def parseSuccessOrFailure(
      updates: Vector[BlockchainUpdate]): BlockchainUpdate = {
    require(updates.nonEmpty,
            s"Cannot parse success or failure if we don't have any updates!")
    val successfulTipOpt: Option[BlockchainUpdate] = {
      updates.find {
        case update: BlockchainUpdate =>
          update.isInstanceOf[BlockchainUpdate.Successful]
      }
    }

    successfulTipOpt match {
      case Some(update) => update
      case None         =>
        //if we didn't successfully connect a tip, just take the first failure we see
        updates.find {
          case update: BlockchainUpdate =>
            update.isInstanceOf[BlockchainUpdate.Failed]
        }.get
    }
  }
}
