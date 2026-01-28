package org.bitcoins.crypto.frost

import org.bitcoins.crypto.{
  ECPublicKey,
  Factory,
  NetworkElement,
  SecpPoint,
  SecpPointFinite
}
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
case class FrostNoncePub(bytes: ByteVector) extends NetworkElement {
  require(bytes.length == 66,
          s"FrostNonceAgg must be 66 bytes, got: ${bytes.length}")

  /** Helper function to parse [[SecpPointInfinity]] case as that is not a valid
    * public key
    */
  private def parse(bytes: ByteVector): SecpPoint = {
    SecpPoint.fromBytes(bytes)
  }
  def r1: SecpPoint = {
    parse(bytes.take(33))
  }

  def r2: SecpPoint = {
    parse(bytes.takeRight(33))
  }
}

object FrostNoncePub extends Factory[FrostNoncePub] {
  override def fromBytes(bytes: ByteVector): FrostNoncePub = {
    new FrostNoncePub(bytes)
  }

  def apply(r1: SecpPointFinite, r2: SecpPointFinite): FrostNoncePub = {
    FrostNoncePub(r1.toPublicKey.bytes ++ r2.toPublicKey.bytes)
  }

  def apply(key1: ECPublicKey, key2: ECPublicKey): FrostNoncePub = {
    require(key1.isCompressed && key2.isCompressed)
    FrostNoncePub(key1.bytes ++ key2.bytes)
  }
}
