package org.bitcoins.core.api.chain

import org.bitcoins.core.api.chain.db.BlockHeaderDb
import org.bitcoins.core.protocol.blockchain.{BitcoinChainParams, BlockHeader}
import org.bitcoins.core.util.SeqWrapper
import org.bitcoins.crypto.DoubleSha256DigestBE

import scala.annotation.tailrec

// INTERNAL NOTE: Due to changes in the Scala collections in 2.13 this
// class and its companion object
// has to be implemented separately for the different Scala versions.
// The general idea is that all three implement a collection, but slightly
// different ones (the one that the 2.12 and 2.11 versions implement got
// removed in 2.13). The most interesting method is `compObjectFromHeaders`.
// This is a method that's meant to represent a `fromHeaders` method on the
// companion object. Because Scala has restrictions on where to place companion
// objects (they have to be in the same file as the trait/class), this was
// the least ugly workaround I could come up with.

/** In memory implementation of a blockchain This data structure maintains the
  * state of a blockchain in memory, the headers can be accessed with
  * [[headers]]. The headers are stored with the most recent header at index 0,
  * the second most recent header at index 1 etc You can walk the chain by
  * {{{
  *   headers.map(h => println(h))
  * }}}
  */
private[chain] trait BaseBlockChain extends SeqWrapper[BlockHeaderDb] {
  require(headers.nonEmpty, s"Cannot have empty Blockchain")
  require(
    headers.size == 1 || headers(1).height == tip.height - 1,
    s"Headers must be in descending order, got ${headers.take(5)}"
  )

  lazy val tip: BlockHeaderDb = headers.head

  /** The height of the chain */
  lazy val height: Int = tip.height

  def headers: Vector[BlockHeaderDb]

  override protected lazy val wrapped: Vector[BlockHeaderDb] = headers

//  def find(predicate: BlockHeaderDb => Boolean): Option[BlockHeaderDb]

  /** Finds a block header at a given height */
  def findAtHeight(height: Int): Option[BlockHeaderDb] =
    find(_.height == height)

  /** Splits the blockchain at the header, returning a new blockchain where the
    * best tip is the given header
    */
  def fromHeader(header: BlockHeaderDb): Option[Blockchain] = {
    val headerIdxOpt = findHeaderIdx(header.hashBE)
    headerIdxOpt.map { idx =>
      val newChain = Blockchain.fromHeaders(headers.splitAt(idx)._2)
      require(newChain.tip == header)
      newChain
    }
  }

  /** Unsafe version for
    * [[org.bitcoins.chain.blockchain.Blockchain.fromHeader() fromHeader]] that
    * can throw [[NoSuchElementException]]
    */
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

  override def toString: String = {
    s"BaseBlockchain(tip=${tip},length=${length})"
  }
}

private[chain] trait BaseBlockChainCompObject {
  def fromHeaders(
      headers: scala.collection.immutable.Seq[BlockHeaderDb]
  ): Blockchain

  /** Iterates through each given blockchains attempting to connect the given
    * headers to that chain
    *
    * @return
    *   The final updates for each chain
    */
  def connectHeadersToChains(
      headers: Vector[BlockHeader],
      blockchains: Vector[Blockchain],
      tipValidationApi: TipValidationApi,
      chainParams: BitcoinChainParams
  ): Vector[BlockchainUpdate] = {

    val initUpdates: Vector[BlockchainUpdate] = blockchains.map { blockchain =>
      BlockchainUpdate.Successful(blockchain, Vector.empty)
    }

    headers.foldLeft(initUpdates) { (lastUpdates, h) =>
      lastUpdates
        .flatMap { lastUpdate =>
          val connectTipResult =
            tipValidationApi.connectTip(
              header = h,
              blockchain = lastUpdate.blockchain,
              chainParams
            )
          parseConnectTipResult(connectTipResult, lastUpdate)
        }
    }
  }

  /** Parses a connect tip result, and depending on the result it
    *   1. Extends the current chain by one block 2. Causes a re-org, which
    *      returns the old best tip and the new competing chain 3. Fails to
    *      connect tip, in which case it returns the old best chain
    */
  private def parseConnectTipResult(
      connectTipResult: ConnectTipResult,
      lastUpdate: BlockchainUpdate
  ): Vector[BlockchainUpdate] = {
    lastUpdate match {
      case _: BlockchainUpdate.Successful =>
        connectTipResult match {
          case ConnectTipResult.ExtendChain(tipUpdateResult, newChain) =>
            val update = BlockchainUpdate.Successful(
              newChain,
              tipUpdateResult.headerDb +: lastUpdate.successfulHeaders
            )
            Vector(update)

          case ConnectTipResult.Reorg(tipUpdateResult, newChain) =>
            val competingUpdate = BlockchainUpdate.Successful(
              newChain,
              tipUpdateResult.headerDb +: lastUpdate.successfulHeaders
            )
            Vector(lastUpdate, competingUpdate)
          case ConnectTipResult.BadTip(tipUpdateResult) =>
            val failedUpdate = BlockchainUpdate.Failed(
              lastUpdate.blockchain,
              lastUpdate.successfulHeaders,
              tipUpdateResult.header,
              tipUpdateResult
            )
            Vector(failedUpdate)

        }

      case f: BlockchainUpdate.Failed => Vector(f)
    }

  }

  /** Walks backwards from the current header searching through ancestors if
    * [[current.previousBlockHashBE]] is in [[ancestors]] This does not validate
    * other things such as POW.
    */
  final def connectWalkBackwards(
      current: BlockHeaderDb,
      ancestors: Vector[BlockHeaderDb]
  ): Vector[BlockHeaderDb] = {
    val groupByHeight: Map[Int, Vector[BlockHeaderDb]] = {
      ancestors.groupBy(_.height)
    }

    @tailrec
    def loop(
        current: BlockHeaderDb,
        accum: Vector[BlockHeaderDb]
    ): Vector[BlockHeaderDb] = {
      val prevHeight = current.height - 1
      val possibleHeadersOpt: Option[Vector[BlockHeaderDb]] =
        groupByHeight.get(prevHeight)

      val prevHeaderOpt = possibleHeadersOpt.flatMap(
        _.find(_.hashBE == current.previousBlockHashBE)
      )
      prevHeaderOpt match {
        case Some(prevHeader) =>
          loop(prevHeader, current +: accum)
        case None =>
          current +: accum
      }
    }

    loop(current, Vector.empty)
  }

  /** Walks backwards from a child header reconstructing a blockchain This
    * validates things like POW, difficulty change etc.
    */
  def reconstructFromHeaders(
      childHeader: BlockHeaderDb,
      ancestors: Vector[BlockHeaderDb],
      tipValidationApi: TipValidationApi,
      chainParams: BitcoinChainParams
  ): Vector[Blockchain] = {
    // now all hashes are connected correctly forming a
    // valid blockchain in term of hashes connected to each other
    val orderedHeaders =
      connectWalkBackwards(current = childHeader, ancestors = ancestors)

    val initBlockchainOpt = {
      if (orderedHeaders.isEmpty || orderedHeaders.length == 1) {
        // for the case of _ +: Vector() this means only our
        // child header is in the chain, which means we
        // weren't able to form a blockchain
        None
      } else {
        // find our first header as we need it's Db representation
        // rather than just the raw header
        val dbOpt = ancestors.find(_.hashBE == orderedHeaders.head.hashBE)
        Some(Blockchain.fromHeaders(Vector(dbOpt.get)))
      }
    }

    // now let's connect headers
    val blockchainUpdateOpt = initBlockchainOpt.map { initBlockchain =>
      Blockchain.connectHeadersToChains(
        headers = orderedHeaders.tail.map(_.blockHeader),
        blockchains = Vector(initBlockchain),
        tipValidationApi = tipValidationApi,
        chainParams = chainParams
      )
    }

    blockchainUpdateOpt match {
      case Some(v) => v.map(_.blockchain)
      case None    => Vector.empty
    }
  }
}
