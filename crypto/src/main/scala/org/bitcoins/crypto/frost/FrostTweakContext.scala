package org.bitcoins.crypto.frost
import org.bitcoins.crypto.musig.{MuSigTweakContext, ParityMultiplier, Pos}
import org.bitcoins.crypto.{
  ECPublicKey,
  FieldElement,
  SecpPointFinite,
  XOnlyPubKey
}

/** A FROST tweak context.
  *
  * Holds the current aggregate public key point `q`, the scalar tweak
  * accumulator `tacc`, and the parity accumulator `gacc`. This context
  * encapsulates the minimal state needed to apply one or more tweaks to an
  * aggregate key as described in the FROST specification (BIP-FROST). The
  * implementation is compatible with MuSig2 tweak semantics so the MuSig tweak
  * logic can be reused.
  *
  * @param Q
  *   aggregate public key point (finite curve point)
  * @param tacc
  *   scalar tweak accumulator (field element)
  * @param gacc
  *   parity accumulator used for x-only parity tracking
  */
case class FrostTweakContext(
    Q: SecpPointFinite,
    tacc: FieldElement,
    gacc: ParityMultiplier) {

  def getXOnlyPubKey: XOnlyPubKey = {
    Q.toPublicKey.toXOnly
  }

  def getPlainPubKey: ECPublicKey = {
    Q.toPublicKey
  }

  /** Apply a tweak to this context and return the updated context.
    *
    * This performs the tweak operation as specified by BIP-FROST (and
    * equivalently by MuSig2): it updates the aggregate public key and the
    * associated accumulators. The `tweak` parameter is a scalar (field element)
    * to apply; `isXOnly` indicates whether the tweak was computed with respect
    * to an x-only representation and therefore may affect parity handling.
    *
    * The returned `FrostTweakContext` contains the new aggregate point `q`, the
    * updated tweak accumulator `tacc`, and the updated parity accumulator
    * `gacc`.
    *
    * @param tweak
    *   scalar tweak to apply
    * @param isXOnly
    *   true when the tweak was derived from an x-only pubkey
    */
  def applyTweak(tweak: FieldElement, isXOnly: Boolean): FrostTweakContext = {
    FrostTweakContext.applyTweak(this, tweak, isXOnly)
  }
}

object FrostTweakContext {

  /** Initializes a tweak context from the given public key */
  def apply(key: ECPublicKey): FrostTweakContext = {
    val point = key.toPoint
    FrostTweakContext(point, tacc = FieldElement.zero, gacc = Pos)
  }

  /** Apply a tweak to the given context and return the updated context.
    *
    * This performs the tweak operation as specified by BIP-FROST (and
    * equivalently by MuSig2): it updates the aggregate public key and the
    * associated accumulators. The `tweak` parameter is a scalar (field element)
    * to apply; `isXOnly` indicates whether the tweak was computed with respect
    * to an x-only representation and therefore may affect parity handling.
    *
    * The returned `FrostTweakContext` contains the new aggregate point `q`, the
    * updated tweak accumulator `tacc`, and the updated parity accumulator
    * `gacc`.
    *
    * @param tweak
    *   scalar tweak to apply
    * @param isXOnly
    *   true when the tweak was derived from an x-only pubkey
    */
  def applyTweak(
      tweakCtx: FrostTweakContext,
      tweak: FieldElement,
      isXOnlyTweak: Boolean): FrostTweakContext = {
    // piggy back of musig implementation as they are the same
    val (aggKey, musigTweak) = MuSigTweakContext
      .apply(parityAcc = tweakCtx.gacc, tweakAcc = tweakCtx.tacc)
      .applyTweak(
        tweak = org.bitcoins.crypto.musig.MuSigTweak(
          tweak = tweak,
          isXOnlyT = isXOnlyTweak
        ),
        aggPubKey = tweakCtx.getPlainPubKey
      )
    FrostTweakContext(
      Q = aggKey.toPoint,
      tacc = musigTweak.tweakAcc,
      gacc = musigTweak.parityAcc
    )
  }
}
