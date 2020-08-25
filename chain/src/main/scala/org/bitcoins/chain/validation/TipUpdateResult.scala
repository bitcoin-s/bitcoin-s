package org.bitcoins.chain.validation

import org.bitcoins.core.api.chain.db.BlockHeaderDb
import org.bitcoins.core.protocol.blockchain.BlockHeader

/** Represents the result of updating the chain with
  * the given header
  */
sealed abstract class TipUpdateResult {
  def header: BlockHeader
}

object TipUpdateResult {

  /** Indicates we successfully update the chain tip with this header */
  case class Success(headerDb: BlockHeaderDb) extends TipUpdateResult {
    override def header = headerDb.blockHeader
  }

  sealed abstract class Failure extends TipUpdateResult

  /** Means that [[org.bitcoins.core.protocol.blockchain.BlockHeader.previousBlockHashBE previousBlockHashBE]] was incorrect */
  case class BadPreviousBlockHash(override val header: BlockHeader)
      extends Failure {

    override def toString: String =
      s"BadPreviousBlockHash(hash=${header.hashBE}, previous=${header.previousBlockHashBE})"
  }

  /** Means that [[org.bitcoins.core.protocol.blockchain.BlockHeader.nBits nBits]] was invalid */
  case class BadPOW(override val header: BlockHeader) extends Failure

  /** Means that [[org.bitcoins.core.protocol.blockchain.BlockHeader.nonce nonce]] was invalid */
  case class BadNonce(override val header: BlockHeader) extends Failure
}
