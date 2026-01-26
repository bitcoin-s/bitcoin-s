package org.bitcoins.crypto.musig

import org.bitcoins.crypto.{CryptoParams, ECPublicKey, FieldElement, OddParity}

/** Represents the total tweak sum and net parity multiplier after applying all
  * tweaks
  */
case class MuSigTweakContext(
    parityAcc: ParityMultiplier,
    tweakAcc: FieldElement) {

  /** Adds tweak to tweakAcc and aggPubKey changing parityAcc if necessary */
  def applyTweak(
      tweak: MuSigTweak,
      aggPubKey: ECPublicKey): (ECPublicKey, MuSigTweakContext) = {
    println(s"Applying tweak: $tweak to aggPubKey: $aggPubKey")
    val parityMult =
      if (tweak.isXOnlyT && aggPubKey.parity == OddParity) Neg
      else Pos

    val newAggPubKey =
      parityMult
        .modify(aggPubKey)
        .add(CryptoParams.getG.multiply(tweak.tweak))
    val newParityAcc = parityAcc.multiply(parityMult)
    val newTweakAcc = parityMult.modify(tweakAcc).add(tweak.tweak)
    println(s"newAggPubKey=$newAggPubKey")
    (newAggPubKey, MuSigTweakContext(newParityAcc, newTweakAcc))
  }
}

object MuSigTweakContext {

  /** The MuSigTweakContext for when there are no tweaks */
  val empty: MuSigTweakContext =
    MuSigTweakContext(Pos, FieldElement.zero)
}
