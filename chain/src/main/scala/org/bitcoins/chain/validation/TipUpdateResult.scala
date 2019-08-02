package org.bitcoins.chain.validation

import org.bitcoins.chain.models.BlockHeaderDb
import org.bitcoins.core.protocol.blockchain.BlockHeader

/** Represents the result of updating the chain with
  * the given header
  */
sealed abstract class TipUpdateResult

object TipUpdateResult {

  /** Indicates we successfully update the chain tip with this header */
  case class Success(header: BlockHeaderDb) extends TipUpdateResult

  sealed abstract class Failure extends TipUpdateResult {
    def header: BlockHeader
  }

  /** Means that [[org.bitcoins.core.protocol.blockchain.BlockHeader.previousBlockHashBE previousBlockHashBE]] was incorrect */
  case class BadPreviousBlockHash(header: BlockHeader) extends Failure {
    override def toString: String =
      s"BadPreviousBlockHash(hash=${header.hashBE}, previous=${header.previousBlockHashBE})"
  }

  /** Means that [[org.bitcoins.core.protocol.blockchain.BlockHeader.nBits nBits]] was invalid */
  case class BadPOW(header: BlockHeader) extends Failure

  /** Means that [[org.bitcoins.core.protocol.blockchain.BlockHeader.nonce nonce]] was invalid */
  case class BadNonce(header: BlockHeader) extends Failure
}
