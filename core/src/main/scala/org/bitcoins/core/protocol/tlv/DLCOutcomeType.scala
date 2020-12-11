package org.bitcoins.core.protocol.tlv

import org.bitcoins.crypto.CryptoUtil
import scodec.bits.ByteVector

/** Represents a DLC event that could be signed by an oracle */
sealed trait DLCOutcomeType {

  /** The ByteVectors to be signed by the oracle using pre-committed nonces */
  def serialized: Vector[ByteVector]
}

/** An outcome from an enumerated event type */
case class EnumOutcome(outcome: String) extends DLCOutcomeType {

  override lazy val serialized: Vector[ByteVector] =
    Vector(CryptoUtil.serializeForHash(outcome))
}

/** An outcome from a multi-nonce unsigned numeric event type.
  *
  * If digits.length is less than the the total number of digits to be
  * signed by the oracle then this outcome represents all outcomes prefixed
  * by the given digits.
  */
case class UnsignedNumericOutcome(digits: Vector[Int]) extends DLCOutcomeType {

  override lazy val serialized: Vector[ByteVector] =
    digits.map(digit => CryptoUtil.serializeForHash(digit.toString))
}
