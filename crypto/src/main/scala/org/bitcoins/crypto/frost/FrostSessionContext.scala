package org.bitcoins.crypto.frost
import org.bitcoins.crypto.FieldElement
import scodec.bits.ByteVector

case class FrostSessionContext(
    signingContext: FrostSigningContext,
    aggNonce: FrostNonce,
    v: Long,
    tweaks: Vector[FieldElement],
    isXOnly: Vector[Boolean],
    message: ByteVector) {
  require(v >= 0 && v < 4294967296L,
          s"v must be in the range [0, 2^32 - 1], got: $v")
  require(tweaks.length == v.toInt,
          s"Number of tweaks ${tweaks.length} must equal v $v")

}
