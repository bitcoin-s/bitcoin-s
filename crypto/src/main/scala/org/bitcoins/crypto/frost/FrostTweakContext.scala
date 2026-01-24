package org.bitcoins.crypto.frost
import org.bitcoins.crypto.musig.{MuSigTweakContext, ParityMultiplier, Pos}
import org.bitcoins.crypto.{
  ECPublicKey,
  FieldElement,
  SecpPointFinite,
  XOnlyPubKey
}

/** Frost Tweak context as defined here:
  * https://github.com/bitcoin/bips/blob/ec46a20323840b1a6aba83bc2d18b34dd0811245/bip-frost-signing.md#tweak-context
  */
case class FrostTweakContext(
    q: SecpPointFinite,
    tacc: FieldElement,
    gacc: ParityMultiplier) {

  def getXOnlyPubKey: XOnlyPubKey = {
    q.toPublicKey.toXOnly
  }

  def getPlainPubKey: ECPublicKey = {
    q.toPublicKey
  }
}

object FrostTweakContext {

  /** Initializes a tweak context from the given public key */
  def apply(key: ECPublicKey): FrostTweakContext = {
    val point = key.toPoint match {
      case p: SecpPointFinite => p
      case _ =>
        throw new IllegalArgumentException(
          s"Public key must not be point at infinity")
    }
    FrostTweakContext(point, tacc = FieldElement.zero, gacc = Pos)
  }

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
      q = aggKey.toPoint,
      tacc = musigTweak.tweakAcc,
      gacc = musigTweak.parityAcc
    )
  }
}
