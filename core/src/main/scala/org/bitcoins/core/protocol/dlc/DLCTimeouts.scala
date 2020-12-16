package org.bitcoins.core.protocol.dlc

import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BlockStamp.{BlockHeight, BlockTime}
import org.bitcoins.core.protocol.BlockTimeStamp
import org.bitcoins.crypto.{Factory, NetworkElement}
import scodec.bits.ByteVector

/** @param contractMaturity The CLTV in milliseconds when a signature is expected
  * @param contractTimeout The CLTV timeout in milliseconds after which the refund tx is valid
  */
case class DLCTimeouts(
    contractMaturity: BlockTimeStamp,
    contractTimeout: BlockTimeStamp
) extends NetworkElement {
  (contractMaturity, contractTimeout) match {
    case (_: BlockTime, _: BlockTime) =>
      if (contractMaturity.toUInt32 >= contractTimeout.toUInt32)
        throw new IllegalArgumentException(
          s"contract must mature before it expires, ${contractTimeout.toUInt32} >= ${contractMaturity.toUInt32}")
    case (_: BlockHeight, _: BlockHeight) =>
      if (contractMaturity.toUInt32 >= contractTimeout.toUInt32)
        throw new IllegalArgumentException(
          s"contract must mature before it expires, ${contractTimeout.toUInt32} >= ${contractMaturity.toUInt32}")
    case (_: BlockTime, _: BlockHeight) | (_: BlockHeight, _: BlockTime) => ()
  }

  override def bytes: ByteVector = {
    contractMaturity.toUInt32.bytes ++ contractTimeout.toUInt32.bytes
  }
}

object DLCTimeouts extends Factory[DLCTimeouts] {

  override def fromBytes(bytes: ByteVector): DLCTimeouts = {
    require(bytes.size == 8, s"A DLCTimeouts is exactly 12 bytes, got $bytes")

    val (maturityBytes, timeoutBytes) = bytes.splitAt(4)

    val contractMaturity =
      BlockTimeStamp.fromUInt32(UInt32(maturityBytes))
    val contractTimeout =
      BlockTimeStamp.fromUInt32(UInt32(timeoutBytes))
    DLCTimeouts(contractMaturity, contractTimeout)
  }
}
