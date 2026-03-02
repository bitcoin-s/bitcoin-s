package org.bitcoins.crypto.musig

import org.bitcoins.crypto.{
  CryptoParams,
  ECPublicKey,
  FieldElement,
  OddParity,
  SecpPointFinite
}

/** Represents the total tweak sum and net parity multiplier after applying all
  * tweaks
  */
case class MuSigTweakContext(
    Q: SecpPointFinite,
    parityAcc: ParityMultiplier,
    tweakAcc: FieldElement) {

  /** Adds tweak to tweakAcc and aggPubKey changing parityAcc if necessary */
  def applyTweak(tweak: MuSigTweak): MuSigTweakContext = {
    val aggPubKey = Q.toPublicKey
    val parityMult =
      if (tweak.isXOnlyT && aggPubKey.parity == OddParity) Neg
      else Pos

    val newAggPubKey =
      parityMult
        .modify(aggPubKey)
        .add(CryptoParams.getG.multiply(tweak.tweak))
    val newParityAcc = parityAcc.multiply(parityMult)
    val newTweakAcc = parityMult.modify(tweakAcc).add(tweak.tweak)
    MuSigTweakContext(newAggPubKey.toPoint, newParityAcc, newTweakAcc)
  }
}

object MuSigTweakContext {

  /** The MuSigTweakContext for when there are no tweaks */
  def apply(pubKey: ECPublicKey): MuSigTweakContext = {
    MuSigTweakContext(
      Q = pubKey.toPoint,
      parityAcc = Pos,
      tweakAcc = FieldElement.zero
    )
  }
}
