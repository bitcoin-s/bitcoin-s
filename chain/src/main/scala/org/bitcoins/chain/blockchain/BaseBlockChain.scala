package org.bitcoins.chain.blockchain

import org.bitcoins.chain.models.BlockHeaderDb
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.ChainVerificationLogger
import org.bitcoins.chain.validation.TipUpdateResult
import org.bitcoins.chain.validation.TipValidation
import scala.annotation.tailrec

// INTERNAL NOTE: Due to changes in the Scala collections in 2.13 this
// class and its companion object
// has to be implemented separetely for the different Scala versions.
// The general idea is that all three implement a collection, but slightly
// different ones (the one that the 2.12 and 2.11 versions implement got
// removed in 2.13). The most interesting method is `compObjectFromHeaders`.
// This is a method that's meant to represent a `fromHeaders` method on the
// companion object. Because Scala has restrictions on where to place companion
// objects (they have to be in the same file as the trait/class), this was
// the least ugly workaround I could come up with.

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
private[blockchain] trait BaseBlockChain {

  protected[blockchain] def compObjectfromHeaders(
      headers: scala.collection.immutable.Seq[BlockHeaderDb]): Blockchain

  val tip: BlockHeaderDb = headers.head

  /** The height of the chain */
  val height: Int = tip.height

  val length: Int = headers.length

  def apply(idx: Int): BlockHeaderDb = headers(idx)

  def headers: Vector[BlockHeaderDb]

  def find(predicate: BlockHeaderDb => Boolean): Option[BlockHeaderDb]

  /** Finds a block header at a given height */
  def findAtHeight(height: Int): Option[BlockHeaderDb] =
    find(_.height == height)

  /** Splits the blockchain at the header, returning a new blockchain where the best tip is the given header */
  def fromHeader(header: BlockHeaderDb): Option[Blockchain] = {
    val headerIdxOpt = headers.zipWithIndex.find(_._1 == header)
    headerIdxOpt.map {
      case (header, idx) =>
        val newChain = this.compObjectfromHeaders(headers.splitAt(idx)._2)
        require(newChain.tip == header)
        newChain
    }
  }

  /** Unsafe version for [[org.bitcoins.chain.blockchain.Blockchain.fromHeader() fromHeader]] that can throw [[NoSuchElementException]] */
  def fromValidHeader(header: BlockHeaderDb): Blockchain = {
    fromHeader(header).get
  }
}

private[blockchain] trait BaseBlockChainCompObject
    extends ChainVerificationLogger {

  def fromHeaders(
      headers: scala.collection.immutable.Seq[BlockHeaderDb]): Blockchain

  /**
    * Attempts to connect the given block header with the given blockchain
    * @param header the block header to connect to our chain
    * @param blockchain the blockchain we are attempting to connect to
    */
  def connectTip(header: BlockHeader, blockchain: Blockchain)(
      implicit conf: ChainAppConfig): ConnectTipResult = {
    logger.debug(
      s"Attempting to add new tip=${header.hashBE.hex} with prevhash=${header.previousBlockHashBE.hex} to chain")

    val tipResult: ConnectTipResult = {
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
          val failed = ConnectTipResult.BadTip(err)
          failed

        case Some((prevBlockHeader, prevHeaderIdx)) =>
          //found a header to connect to!
          logger.debug(
            s"Attempting to add new tip=${header.hashBE.hex} with prevhash=${header.previousBlockHashBE.hex} to chain")
          val chain = blockchain.fromValidHeader(prevBlockHeader)
          val tipResult =
            TipValidation.checkNewTip(newPotentialTip = header, chain)

          tipResult match {
            case success: TipUpdateResult.Success =>
              logger.debug(
                s"Successfully verified=${success.header.hashBE.hex}, connecting to chain")
              val connectionIdx = blockchain.length - prevHeaderIdx

              val oldChain =
                blockchain.takeRight(connectionIdx)
              val newChain =
                Blockchain.fromHeaders(success.headerDb +: oldChain)

              if (connectionIdx != blockchain.length) {
                //means we have a reorg, since we aren't connecting to latest tip
                ConnectTipResult.Reorg(success, newChain)
              } else {
                //we just extended the latest tip
                ConnectTipResult.ExtendChain(success, newChain)
              }
            case fail: TipUpdateResult.Failure =>
              logger.warn(
                s"Could not verify header=${header.hashBE.hex}, reason=$fail")
              ConnectTipResult.BadTip(fail)
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
    logger.debug(
      s"Attempting to connect ${headers.length} headers to ${blockchains.length} blockchains")

    @tailrec
    def loop(
        headersToProcess: Vector[BlockHeader],
        lastUpdates: Vector[BlockchainUpdate]): Vector[BlockchainUpdate] = {
      headersToProcess match {
        case h +: t =>
          val newUpdates: Vector[BlockchainUpdate] = lastUpdates
            .flatMap { lastUpdate =>
              val connectTipResult =
                Blockchain.connectTip(header = h,
                                      blockchain = lastUpdate.blockchain)
              parseConnectTipResult(connectTipResult, lastUpdate)
            }

          loop(headersToProcess = t, lastUpdates = newUpdates)
        case Vector() =>
          lastUpdates
      }
    }

    val initUpdates = blockchains.map { blockchain =>
      BlockchainUpdate.Successful(blockchain, Vector.empty)
    }

    loop(headers, initUpdates)
  }

  /** Parses a connect tip result, and depending on the result it
    * 1. Extends the current chain by one block
    * 2. Causes a re-org, which returns the old best tip and the new competing chain
    * 3. Fails to connect tip, in which case it returns the old best chain
    * */
  private def parseConnectTipResult(
      connectTipResult: ConnectTipResult,
      lastUpdate: BlockchainUpdate): Vector[BlockchainUpdate] = {
    lastUpdate match {
      case _: BlockchainUpdate.Successful =>
        connectTipResult match {
          case ConnectTipResult.ExtendChain(tipUpdateResult, newChain) =>
            val update = BlockchainUpdate.Successful(
              newChain,
              tipUpdateResult.headerDb +: lastUpdate.successfulHeaders)
            Vector(update)

          case ConnectTipResult.Reorg(tipUpdateResult, newChain) =>
            val competingUpdate = BlockchainUpdate.Successful(
              newChain,
              tipUpdateResult.headerDb +: lastUpdate.successfulHeaders)
            Vector(lastUpdate, competingUpdate)
          case ConnectTipResult.BadTip(tipUpdateResult) =>
            val failedUpdate = BlockchainUpdate.Failed(
              lastUpdate.blockchain,
              lastUpdate.successfulHeaders,
              tipUpdateResult.header,
              tipUpdateResult)
            Vector(failedUpdate)

        }

      case f: BlockchainUpdate.Failed => Vector(f)
    }

  }

}
