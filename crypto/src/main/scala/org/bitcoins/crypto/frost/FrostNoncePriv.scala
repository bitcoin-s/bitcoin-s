package org.bitcoins.crypto.frost

import org.bitcoins.crypto.{
  ECPrivateKey,
  Factory,
  FieldElement,
  MaskedToString,
  NetworkElement
}
import scodec.bits.ByteVector

/** FROST nonce (private) representation.
  *
  * This class wraps the serialized form of two secret nonces concatenated
  * together. Each secret nonce is represented as a 32-byte big-endian scalar (a
  * field element modulo the curve order), so the full representation is 64
  * bytes: k1 || k2.
  *
  * Layout:
  *   - bytes[0:32) => big-endian scalar encoding of k1
  *   - bytes[32:64) => big-endian scalar encoding of k2
  *
  * In the FROST protocol (and MuSig2), signers generate two secret nonces per
  * signing session. Their corresponding public nonces are R1 = k1*G and R2 =
  * k2*G, and the test vectors store the concatenation of the two private nonces
  * in this format. The two nonces enable construction of the final signing
  * nonce as a linear combination (e.g. R = R1_agg + c*R2_agg), where c is a
  * hash-derived coefficient that depends on the aggregated nonces, aggregate
  * public key, and message.
  *
  * Security notes:
  *   - Each 32-byte chunk should be interpreted as a scalar and validated
  *     according to your scalar/field representation (reduction modulo the
  *     curve order if needed).
  *   - Nonces must be treated as secret material: zero them in memory after use
  *     and avoid leaking them (this class implements `MaskedToString` to
  *     prevent accidental exposure in logs).
  *
  * @param bytes
  *   the 64-byte concatenation of two 32-byte secret nonces (k1||k2)
  */
case class FrostNoncePriv(bytes: ByteVector)
    extends NetworkElement
    with MaskedToString {
  require(bytes.length == 64, s"FrostNoncePriv must be 64 bytes, got: $bytes")
  override def toStringSensitive: String = s"FrostNoncePriv(${bytes.toHex})"
  def k1: ECPrivateKey = ECPrivateKey.fromBytes(bytes.slice(0, 32))
  def k2: ECPrivateKey = ECPrivateKey.fromBytes(bytes.slice(32, 64))
  def toNoncePub: FrostNoncePub = {
    FrostNoncePub(k1.publicKey, k2.publicKey)
  }
}

object FrostNoncePriv extends Factory[FrostNoncePriv] {
  def fromBytes(bytes: ByteVector): FrostNoncePriv = {
    new FrostNoncePriv(bytes)
  }

  def apply(k1: ECPrivateKey, k2: ECPrivateKey): FrostNoncePriv = {
    fromBytes(k1.bytes ++ k2.bytes)
  }

  def apply(fe1: FieldElement, fe2: FieldElement): FrostNoncePriv = {
    FrostNoncePriv(ECPrivateKey.fromFieldElement(fe1),
                   ECPrivateKey.fromFieldElement(fe2))
  }
}
