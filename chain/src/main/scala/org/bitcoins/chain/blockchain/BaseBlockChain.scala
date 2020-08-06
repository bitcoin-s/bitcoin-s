package org.bitcoins.chain.blockchain

import org.bitcoins.chain.ChainVerificationLogger
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.BlockHeaderDb
import org.bitcoins.chain.validation.{TipUpdateResult, TipValidation}
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.core.util.SeqWrapper
import org.bitcoins.crypto.DoubleSha256DigestBE

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
  */
private[blockchain] trait BaseBlockChain extends SeqWrapper[BlockHeaderDb] {

  protected[blockchain] def compObjectfromHeaders(
      headers: scala.collection.immutable.Seq[BlockHeaderDb]): Blockchain

  val tip: BlockHeaderDb = headers.head

  require(headers.size <= 1 || headers(1).height == tip.height - 1,
          s"Headers must be in descending order, got ${headers.take(5)}")

  /** The height of the chain */
  val height: Int = tip.height

  def headers: Vector[BlockHeaderDb]

  override protected lazy val wrapped: Vector[BlockHeaderDb] = headers

  def find(predicate: BlockHeaderDb => Boolean): Option[BlockHeaderDb]

  /** Finds a block header at a given height */
  def findAtHeight(height: Int): Option[BlockHeaderDb] =
    find(_.height == height)

  /** Splits the blockchain at the header, returning a new blockchain where the best tip is the given header */
  def fromHeader(header: BlockHeaderDb): Option[Blockchain] = {
    val headerIdxOpt = findHeaderIdx(header.hashBE)
    headerIdxOpt.map { idx =>
      val newChain = this.compObjectfromHeaders(headers.splitAt(idx)._2)
      require(newChain.tip == header)
      newChain
    }
  }

  /** Unsafe version for [[org.bitcoins.chain.blockchain.Blockchain.fromHeader() fromHeader]] that can throw [[NoSuchElementException]] */
  def fromValidHeader(header: BlockHeaderDb): Blockchain = {
    fromHeader(header).get
  }

  /** Finds a header index by the given hash */
  def findHeaderIdx(hashBE: DoubleSha256DigestBE): Option[Int] = {

    @tailrec
    def loop(idx: Int): Option[Int] = {
      if (idx >= headers.size)
        None
      else {
        val header = headers(idx)
        if (header.hashBE == hashBE)
          Some(idx)
        else
          loop(idx + 1)
      }
    }

    loop(0)
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
  def connectTip(header: BlockHeader, blockchain: Blockchain)(implicit
      conf: ChainAppConfig): ConnectTipResult = {
    logger.debug(
      s"Attempting to add new tip=${header.hashBE.hex} with prevhash=${header.previousBlockHashBE.hex} to chain")

    val tipResult: ConnectTipResult = {
      findPrevBlockHeaderIdx(header, blockchain) match {
        case None =>
          logger.warn(
            s"No common ancestor found in the chain to connect to ${header.hashBE}")
          val err = TipUpdateResult.BadPreviousBlockHash(header)
          val failed = ConnectTipResult.BadTip(err)
          failed

        case Some(prevHeaderIdx) =>
          //found a header to connect to!
          val prevBlockHeader = blockchain.headers(prevHeaderIdx)
          logger.debug(
            s"Attempting to add new tip=${header.hashBE.hex} with prevhash=${header.previousBlockHashBE.hex} to chain of ${blockchain.length} headers with tip ${blockchain.tip.hashBE.hex}")
          val chain = blockchain.fromValidHeader(prevBlockHeader)
          val tipResult =
            TipValidation.checkNewTip(newPotentialTip = header, chain)

          tipResult match {
            case success: TipUpdateResult.Success =>
              logger.debug(
                s"Successfully verified=${success.header.hashBE.hex}, connecting to chain")
              val connectionIdx = blockchain.length - prevHeaderIdx

              // we construct a new blockchain by prepending the headers vector from the old one with the new tip
              // in order to avoid creating unnecessary hidden copies of the blockchain here
              if (connectionIdx != blockchain.length) {
                val newChain = Blockchain(
                  success.headerDb +: blockchain.headers.takeRight(
                    connectionIdx))
                //means we have a reorg, since we aren't connecting to latest tip
                ConnectTipResult.Reorg(success, newChain)
              } else {
                val olderChain = if (blockchain.size < 2016) {
                  blockchain.headers
                } else blockchain.headers.take(2015)
                val newChain = Blockchain(success.headerDb +: olderChain)
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
    *
    * @return The final updates for each chain
    */
  def connectHeadersToChains(
      headers: Vector[BlockHeader],
      blockchains: Vector[Blockchain])(implicit
      chainAppConfig: ChainAppConfig): Vector[BlockchainUpdate] = {
    logger.debug(
      s"Attempting to connect ${headers.length} headers to ${blockchains.length} blockchains")

    val initUpdates: Vector[BlockchainUpdate] = blockchains.map { blockchain =>
      BlockchainUpdate.Successful(blockchain, Vector.empty)
    }

    headers.foldLeft(initUpdates) { (lastUpdates, h) =>
      lastUpdates
        .flatMap { lastUpdate =>
          val connectTipResult =
            Blockchain.connectTip(header = h,
                                  blockchain = lastUpdate.blockchain)
          parseConnectTipResult(connectTipResult, lastUpdate)
        }
    }
  }

  /** Parses a connect tip result, and depending on the result it
    * 1. Extends the current chain by one block
    * 2. Causes a re-org, which returns the old best tip and the new competing chain
    * 3. Fails to connect tip, in which case it returns the old best chain
    */
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

  /**
    * Finds the parent's index of the given header
    */
  private def findPrevBlockHeaderIdx(
      header: BlockHeader,
      blockchain: Blockchain): Option[Int] = {
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

  /** Walks backwards from the current header searching through ancestors if [[current.previousBlockHashBE]] is in [[ancestors]]
    * This does not validate other things such as POW.
    */
  @tailrec
  final def connectWalkBackwards(
      current: BlockHeaderDb,
      ancestors: Vector[BlockHeaderDb],
      accum: Vector[BlockHeaderDb] = Vector.empty)(implicit
      chainAppConfig: ChainAppConfig): Vector[BlockHeaderDb] = {
    val prevHeaderOpt = ancestors.find(_.hashBE == current.previousBlockHashBE)
    prevHeaderOpt match {
      case Some(h) =>
        connectWalkBackwards(current = h,
                             accum = current +: accum,
                             ancestors = ancestors)
      case None =>
        logger.debug(s"No prev found for $current hashBE=${current.hashBE}")
        current +: accum
    }
  }

  /** Walks backwards from a child header reconstructing a blockchain
    * This validates things like POW, difficulty change etc.
    */
  def reconstructFromHeaders(
      childHeader: BlockHeaderDb,
      ancestors: Vector[BlockHeaderDb])(implicit
      chainAppConfig: ChainAppConfig): Vector[Blockchain] = {
    //now all hashes are connected correctly forming a
    //valid blockchain in term of hashes connected to each other
    val orderedHeaders = connectWalkBackwards(current = childHeader,
                                              accum = Vector.empty,
                                              ancestors = ancestors)

    val initBlockchainOpt = orderedHeaders match {
      case Vector() | _ +: Vector() =>
        //for the case of _ +: Vector() this means only our
        //child header is in the chain, which means we
        //weren't able to form a blockchain
        None
      case h +: _ =>
        //find our first header as we need it's Db representation
        //rather than just the raw header
        val dbOpt = ancestors.find(_.hashBE == h.hashBE)
        Some(Blockchain.fromHeaders(Vector(dbOpt.get)))
    }

    //now let's connect headers
    val blockchainUpdateOpt = initBlockchainOpt.map { initBlockchain =>
      Blockchain.connectHeadersToChains(orderedHeaders.tail.map(_.blockHeader),
                                        Vector(initBlockchain))
    }

    blockchainUpdateOpt match {
      case Some(v) => v.map(_.blockchain)
      case None    => Vector.empty
    }
  }
}
