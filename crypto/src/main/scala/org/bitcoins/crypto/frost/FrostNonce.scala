package org.bitcoins.crypto.frost

import org.bitcoins.crypto.{Factory, NetworkElement}
import scodec.bits.ByteVector

/** FROST nonce (public) representation.
  *
  * This class wraps the serialized form of two SEC-compressed curve points
  * concatenated together. Each compressed point uses 33 bytes, so the full
  * representation is 66 bytes.
  *
  * Layout:
  *   - bytes[0:33) => compressed representation of R1
  *   - bytes[33:66) => compressed representation of R2
  *
  * In the FROST protocol these correspond to the two public nonces (R1, R2)
  * produced by a participant during nonce generation. The `require` in the
  * constructor enforces the fixed-length invariant.
  *
  * Note:
  *   - The byte order is the concatenation of the compressed encodings as
  *     returned by the underlying curve library (typically SEC compressed
  *     form).
  *   - Consumers should parse each 33-byte slice into the appropriate
  *     elliptic-curve point type before using them in point arithmetic.
  *
  * @param bytes
  *   the 66-byte concatenation of two SEC-compressed curve points
  */
case class FrostNonce(bytes: ByteVector) extends NetworkElement {
  require(bytes.length == 66,
          s"FrostNonceAgg must be 66 bytes, got: ${bytes.length}")
}

object FrostNonce extends Factory[FrostNonce] {
  override def fromBytes(bytes: ByteVector): FrostNonce = {
    new FrostNonce(bytes)
  }
}
