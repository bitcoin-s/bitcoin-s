package org.bitcoins.crypto.frost

import org.bitcoins.crypto.{ECPublicKey, FieldElement, SecpPointFinite}
import org.bitcoins.crypto.musig.ParityMultiplier

/** Container of per-session values used during a FROST signing session.
  *
  * This class groups the mutable/derived values required to produce or verify a
  * signature for a single signing session. It includes the current tweak
  * context (aggregate key + accumulators), the participant identifiers and
  * their public shares, and the session-specific scalars/points used in the
  * signing equation.
  *
  * Fields:
  *   - `tweakCtx`: current aggregate key and tweak/parity accumulators (q,
  *     tacc, gacc)
  *   - `ids`: vector of participant identifiers used to index shares
  *   - `pubshares`: each participant's public share (in the same order as
  *     `ids`)
  *   - `b`: session binding or coefficient scalar used when computing the
  *     signer contribution (protocol-specific)
  *   - `r`: the combined nonce point (finite curve point) used in the signature
  *     challenge
  *   - `e`: the challenge scalar (usually derived from R, the aggregate pubkey,
  *     and the message)
  *
  * Convenience accessors `q`, `gacc`, and `tacc` forward to fields in
  * `tweakCtx`.
  */
case class FrostSessionValues(
    tweakCtx: FrostTweakContext,
    ids: Vector[Int],
    pubshares: Vector[ECPublicKey],
    b: FieldElement,
    r: SecpPointFinite,
    e: FieldElement) {
  require(
    ids.length == pubshares.length,
    s"Number of ids ${ids.length} must equal number of pubshares ${pubshares.length}")
  def q: SecpPointFinite = tweakCtx.q

  def gacc: ParityMultiplier = tweakCtx.gacc

  def tacc: FieldElement = tweakCtx.tacc
}
