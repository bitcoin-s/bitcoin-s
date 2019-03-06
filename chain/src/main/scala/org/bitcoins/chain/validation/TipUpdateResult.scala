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

  /** Means that [[header.previousBlockHashBE]] was incorrect */
  case class BadPreviousBlockHash(header: BlockHeader) extends Failure

  /** Means that [[header.nBits]] was invalid */
  case class BadPOW(header: BlockHeader) extends Failure

  /** Means that [[header.nonce]] was invalid */
  case class BadNonce(header: BlockHeader) extends Failure
}
