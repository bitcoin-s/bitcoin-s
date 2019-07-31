package org.bitcoins.chain.blockchain

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
sealed abstract class BlockchainUpdate

object BlockchainUpdate {

  /** The key thing we receive here is [[org.bitcoins.chain.models.BlockHeaderDb BlockHeaderDb]]
    * with a height assigned to it this happens after
    * calling [[org.bitcoins.chain.blockchain.ChainHandler.processHeader ChainHandler.processHeader]]
    */
  case class Successful(blockchain: Blockchain, updatedHeader: BlockHeaderDb)
      extends BlockchainUpdate {
    def height: Long = updatedHeader.height
  }

  case class Failed(
      blockchain: Blockchain,
      failedHeader: BlockHeader,
      tipUpdateFailure: TipUpdateResult.Failure)
      extends BlockchainUpdate
}
