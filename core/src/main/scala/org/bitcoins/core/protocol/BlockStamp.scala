package org.bitcoins.core.protocol

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.number.UInt32

/** This trait represents a point on blockchain, and is used to specify block ranges */
sealed trait BlockStamp

object BlockStamp {
  case class BlockHash(hash: DoubleSha256Digest) extends BlockStamp
  case class BlockHeight(height: Int) extends BlockStamp
  case class BlockTime(time: UInt32) extends BlockStamp
}
