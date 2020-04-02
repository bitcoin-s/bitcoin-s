package org.bitcoins.dlc

import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.{BlockStampWithFuture, NetworkElement}
import org.bitcoins.core.util.Factory
import scodec.bits.ByteVector

/** @param penaltyTimeout The CSV timeout in blocks used in all CETs
  * @param contractMaturity The CLTV in milliseconds when a signature is expected
  * @param contractTimeout The CLTV timeout in milliseconds after which the refund tx is valid
  */
case class DLCTimeouts(
    penaltyTimeout: UInt32,
    contractMaturity: BlockStampWithFuture,
    contractTimeout: BlockStampWithFuture
) extends NetworkElement {
  override def bytes: ByteVector = {
    penaltyTimeout.bytes ++ contractMaturity.toUInt32.bytes ++ contractTimeout.toUInt32.bytes
  }
}

object DLCTimeouts extends Factory[DLCTimeouts] {

  /** The default CSV timeout in blocks used in all CETs */
  final val DEFAULT_PENALTY_TIMEOUT: UInt32 = UInt32(5)

  override def fromBytes(bytes: ByteVector): DLCTimeouts = {
    require(bytes.size == 12, s"A DLCTimeouts is exactly 12 bytes, got $bytes")

    val penalty = UInt32(bytes.take(4))
    val contractMaturity =
      BlockStampWithFuture.fromUInt32(UInt32(bytes.slice(4, 8)))
    val contractTimeout =
      BlockStampWithFuture.fromUInt32(UInt32(bytes.takeRight(4)))
    DLCTimeouts(penalty, contractMaturity, contractTimeout)
  }
}
