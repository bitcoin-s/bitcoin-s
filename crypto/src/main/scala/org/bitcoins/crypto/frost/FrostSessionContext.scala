package org.bitcoins.crypto.frost
import org.bitcoins.crypto.*
import scodec.bits.ByteVector

/** Container for a FROST signing session.
  *
  * This class encapsulates all the information needed for a single FROST
  * signing session, including the signing context (participant information),
  * aggregated nonces, optional tweaks, and the message to be signed.
  *
  * The session context is used to compute session-specific values (via
  * `getSessionValues`) such as:
  *   - `b`: the nonce coefficient used to combine R1 and R2
  *   - `R`: the effective nonce point used in the signature
  *   - `e`: the challenge scalar derived from R, the (tweaked) aggregate public
  *     key, and the message
  *
  * These values are required by both the signing and verification algorithms.
  *
  * @param signingContext
  *   the signing context containing participant information, their public key
  *   shares, and the threshold public key
  * @param aggNonce
  *   the aggregated public nonce from all signing participants (R1_agg, R2_agg)
  * @param tweaks
  *   optional scalar tweaks to apply to the threshold public key (e.g., for
  *   taproot key path spending)
  * @param isXOnly
  *   for each tweak, indicates whether it was derived from an x-only public key
  *   (affects parity handling)
  * @param message
  *   the message bytes to be signed
  */
case class FrostSessionContext(
    signingContext: FrostSigningContext,
    aggNonce: FrostNoncePub,
    tweaks: Vector[FieldElement],
    isXOnly: Vector[Boolean],
    message: ByteVector) {
  def v: Long = tweaks.length

  require(v >= 0 && v < 4294967296L,
          s"v must be in the range [0, 2^32 - 1], got: $v")

  require(isXOnly.length == v.toInt,
          s"isXOnly must have the same length as tweaks (v=$v)")

  def r1: SecpPoint = aggNonce.r1
  def r2: SecpPoint = aggNonce.r2

  /** Computes session-specific values needed for FROST signing and
    * verification.
    *
    * This method derives the following values from the session context:
    *   - **`b`**: the nonce coefficient (binding factor) computed by hashing
    *     the sorted participant IDs, aggregated nonce, (tweaked) threshold
    *     public key, and message. This value binds the nonces to the specific
    *     signing session.
    *   - **`R`**: the effective nonce point computed as R = R1 + b·R2, where R1
    *     and R2 are from the aggregated nonce. If R' (before parity adjustment)
    *     is the point at infinity, the generator G is used instead.
    *   - **`e`**: the challenge scalar computed via BIP-340 challenge hash from
    *     R (x-only), the (tweaked) threshold public key (x-only), and the
    *     message.
    *   - **`tweakCtx`**: the tweak context containing the (tweaked) aggregate
    *     public key Q and the tweak/parity accumulators (tacc, gacc) after
    *     applying all tweaks.
    *
    * The returned `FrostSessionValues` contains all these computed values along
    * with participant IDs and their public key shares. These values are used in
    * both the signing equation (s = k1 + b·k2 + e·λ·d) and the verification
    * equation (s·G = R + e·λ·gacc·g·X).
    *
    * @return
    *   FrostSessionValues containing b, R, e, the tweak context, participant
    *   IDs, and public shares
    * @throws IllegalArgumentException
    *   if the computed b or e equals zero (negligible probability)
    */
  def getSessionValues: FrostSessionValues = {
    val thresholdPk = signingContext.thresholdPubKey
    val initTweakCtx = FrostTweakContext(thresholdPk)
    val tweakCtx =
      1.to(v.toInt).foldLeft(initTweakCtx) { case (tweakCtx, i) =>
        tweakCtx.applyTweak(tweaks(i - 1), isXOnly(i - 1))
      }
    val serializedIds =
      signingContext.participantIds.sorted.foldLeft(ByteVector.empty) {
        case (acc, id) =>
          acc ++ ByteVector.fromLong(id, 4)
      }

    val bHash = FrostUtil.hashFrostNonceCoef(
      serializedIds ++
        aggNonce.bytes ++
        tweakCtx.getXOnlyPubKey.bytes ++
        message
    )
    val b = FieldElement.fromBytes(bHash)

    require(b != FieldElement.zero,
            s"Computed challenge 'b' cannot be zero in FROST signing session")
    val rPrime = r1.add(r2.multiply(b))
    val r = rPrime match {
      case SecpPointInfinity  => CryptoParams.getG
      case p: SecpPointFinite => p.toPublicKey
    }
    val eHash = CryptoUtil.sha256SchnorrChallenge(
      r.toXOnly.bytes ++
        tweakCtx.getXOnlyPubKey.bytes ++
        message
    )
    val e = FieldElement.fromBytes(eHash.bytes)
    require(e != FieldElement.zero,
            s"Computed challenge 'e' cannot be zero in FROST signing session")
    FrostSessionValues(tweakCtx = tweakCtx,
                       ids = signingContext.participantIds,
                       pubshares = signingContext.pubshares,
                       b = b,
                       R = r.toPoint,
                       e = e)
  }
}
