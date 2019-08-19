package org.bitcoins.chain.blockchain

import org.bitcoins.chain.blockchain.BlockchainUpdate.{Failed, Successful}
import org.bitcoins.chain.models.BlockHeaderDb
import org.bitcoins.chain.validation.TipUpdateResult
import org.bitcoins.core.protocol.blockchain.BlockHeader

/** Represents the state of an update to our [[org.bitcoins.chain.blockchain.Blockchain Blockchain]]
  * An example of a successful update is receiving a [[org.bitcoins.core.protocol.blockchain.BlockHeader BlockHeader]] and successfully
  * adding it to our database.
  *
  * An example of a [[org.bitcoins.chain.blockchain.BlockchainUpdate.Failed Failed]] update
  * is when we receive a [[org.bitcoins.core.protocol.blockchain.BlockHeader BlockHeader]] that is invalid and because of a
  * [[org.bitcoins.chain.validation.TipUpdateResult.Failure TipUpdateFailure]]
  * because of [[org.bitcoins.chain.validation.TipUpdateResult.BadPOW BadPOW]] or a
  * [[org.bitcoins.chain.validation.TipUpdateResult.BadNonce BadNonce]] etc
  */
sealed abstract class BlockchainUpdate {
  def successfulHeaders: Vector[BlockHeaderDb]

  def blockchain: Blockchain

  def withSuccessfulHeaders(headers: Vector[BlockHeaderDb]): BlockchainUpdate =
    this match {
      case s: Successful =>
        s.copy(successfulHeaders = headers)
      case f: Failed =>
        f.copy(successfulHeaders = headers)
    }
}

object BlockchainUpdate {

  /** The key thing we receive here is [[org.bitcoins.chain.models.BlockHeaderDb BlockHeaderDb]]
    * with a height assigned to it this happens after
    * calling [[org.bitcoins.chain.blockchain.ChainHandler.processHeaders ChainHandler.processHeaders]]
    */
  case class Successful(
      blockchain: Blockchain,
      successfulHeaders: Vector[BlockHeaderDb])
      extends BlockchainUpdate {
    if (successfulHeaders.nonEmpty) {
      require(blockchain.tip == successfulHeaders.head)
    }
    def height: Long = blockchain.height
  }

  /**
    * Means we failed to update the given blockchain with _ALL_ given headers
    * This means we could have had a partially successful update, with the headers/blockchain
    * returned in this case class
    */
  case class Failed(
      blockchain: Blockchain,
      successfulHeaders: Vector[BlockHeaderDb],
      failedHeader: BlockHeader,
      tipUpdateFailure: TipUpdateResult.Failure)
      extends BlockchainUpdate {
    require(
      !blockchain.contains(failedHeader),
      s"Our blockchain should not contain the failed header=${failedHeader}")

    if (successfulHeaders.nonEmpty) {
      require(
        successfulHeaders.head == blockchain.tip,
        s"Our blockchain.tip should be the first successful header, got=${blockchain.tip} expected=${successfulHeaders.head}"
      )
    }
  }
}
