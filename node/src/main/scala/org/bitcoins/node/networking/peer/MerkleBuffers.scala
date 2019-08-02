package org.bitcoins.node.networking.peer

import org.bitcoins.core.util.BitcoinSLogger
import scala.collection.mutable
import org.bitcoins.core.protocol.blockchain.MerkleBlock
import org.bitcoins.core.protocol.transaction.Transaction

/**
  * A buffer of merkleblocks and the transactions associated with them.
  *
  * When receiving a merkleblock message over the P2P network, the
  * corresponding transactions are sent immediately after. That means
  * we have to correlate the received merkleblocks with the matching
  * transactions.
  *
  * This buffer is responsible for calling the approriate callbacks
  * once a merkle block has received all its transactions.
  */
private[peer] object MerkleBuffers extends BitcoinSLogger {
  private type MerkleBlocksWithTransactions =
    mutable.Map[MerkleBlock, mutable.Builder[Transaction, Vector[Transaction]]]

  private val underlyingMap: MerkleBlocksWithTransactions = mutable.Map.empty

  /** Adds the given merkleblock to the buffer */
  def putMerkle(merkle: MerkleBlock): Unit = {
    val tree = merkle.partialMerkleTree
    val matches = tree.extractMatches

    logger.debug(s"Received merkle block, expecting ${matches.length} TX(s)")

    if (matches.nonEmpty) {
      logger.trace(s"Adding merkleBlock=${merkle.blockHeader.hashBE} to buffer")
      underlyingMap.put(merkle,
                        // it's important to use a collection
                        // type that can call .result() without
                        // clearing the builder
                        Vector.newBuilder)
    } else {
      logger.trace(
        s"Merkleblock=${merkle.blockHeader.hashBE} has no matches, not adding to buffer")
    }

    ()
  }

  /** Attempts to add the given transaction to a corresponding
    * merkleblock in the buffer.
    *
    * @param tx The transaction to (maybe) add to the buffer
    * @param callbacks The callbacks to execute if we're
    *        finished processing a merkleblock
    *
    * @return If the transaction matches a merkle block, returns true.
    *         Otherwise, false.
    */
  def putTx(
      tx: Transaction,
      callbacks: Seq[DataMessageHandler.OnMerkleBlockReceived]): Boolean = {
    val blocksInBuffer = underlyingMap.keys.toList
    logger.trace(s"Looking for transaction=${tx.txIdBE} in merkleblock buffer")
    logger.trace(s"Merkleblocks in buffer: ${blocksInBuffer.length}")
    blocksInBuffer.find { block =>
      val matches = block.partialMerkleTree.extractMatches

      logger.trace(
        s"Block=${block.blockHeader.hashBE} has matches=${matches.map(_.flip)}")

      matches.exists(_ == tx.txId)
    } match {
      case None =>
        logger.debug(
          s"Transaction=${tx.txIdBE} does not belong to any merkle block")
        false
      case Some(key) =>
        handleMerkleMatch(tx, merkleBlock = key, callbacks = callbacks)
    }
  }

  // TODO Scaladoc
  private def handleMerkleMatch(
      transaction: Transaction,
      merkleBlock: MerkleBlock,
      callbacks: Seq[DataMessageHandler.OnMerkleBlockReceived]) = {
    val merkleBlockMatches = merkleBlock.partialMerkleTree.extractMatches
    val merkleHash = merkleBlock.blockHeader.hashBE

    val txHash = transaction.txIdBE

    logger.debug(s"Transaction=$txHash matched merkleBlock=$merkleHash")

    logger.trace(s"Adding transaction=$txHash to buffer")
    val builder = underlyingMap(merkleBlock) // TODO: error handling
    builder += transaction

    val transactionSoFar = builder.result()
    val transactionSoFarCount = transactionSoFar.length
    val matchesCount = merkleBlockMatches.length
    if (transactionSoFarCount == matchesCount) {
      logger.debug(
        s"We've received all transactions ($transactionSoFarCount) for merkleBlock=$merkleHash")

      logger.trace(s"Removing merkle block from buffer")
      underlyingMap.remove(merkleBlock) // TODO: error handling

      logger.trace(s"Calling merkle block callback(s)")
      callbacks.foreach(_.apply(merkleBlock, transactionSoFar))
    } else {
      logger.trace(
        s"We've received $transactionSoFarCount, expecting $matchesCount")
      assert(transactionSoFarCount < matchesCount)
    }
    true
  }
}
