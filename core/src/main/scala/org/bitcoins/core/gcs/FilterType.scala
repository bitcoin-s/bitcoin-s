package org.bitcoins.core.gcs

import org.bitcoins.core.protocol.NetworkElement
import scodec.bits._
import org.bitcoins.core.number.UInt64
import org.bitcoins.core.number.UInt8
import org.bitcoins.core.util.Factory

/**
  * Filter types for BIP157 block content filters
  *
  * @see [[https://github.com/bitcoin/bips/blob/master/bip-0158.mediawiki#block-filters BIP158]]
  */
sealed abstract class FilterType extends NetworkElement {
  val M: UInt64
  val P: UInt8
}

object FilterType extends Factory[FilterType] {

  def fromBytes(bytes: ByteVector): FilterType = bytes match {
    case Basic.bytes => Basic
    case other: ByteVector =>
      throw new IllegalArgumentException(
        s"'${other.toHex}' is not a known filter type")
  }

  /** Currently the only defined filter type */
  final case object Basic extends FilterType {
    val bytes: ByteVector = hex"0x00"

    val M: UInt64 = UInt64(784931)
    val P: UInt8 = UInt8(19)
  }

}
