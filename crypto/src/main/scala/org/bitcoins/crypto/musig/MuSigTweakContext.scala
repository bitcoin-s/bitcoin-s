package org.bitcoins.crypto.musig

import org.bitcoins.crypto.{ECPublicKey, FieldElement, OddParity}

case class MuSigTweakContext(parityAcc: FieldElement, tweakAcc: FieldElement) {

  def applyTweak(
      tweak: MuSigTweak,
      aggPubKey: ECPublicKey): (ECPublicKey, MuSigTweakContext) = {
    val parityMult =
      if (tweak.isXOnlyT && aggPubKey.parity == OddParity)
        FieldElement.orderMinusOne
      else FieldElement.one

    val newAggPubKey = aggPubKey.multiply(parityMult).add(tweak.point)
    val newParityAcc = parityAcc.multiply(parityMult)
    val newTweakAcc = tweakAcc.multiply(parityMult).add(tweak.tweak)
    (newAggPubKey, MuSigTweakContext(newParityAcc, newTweakAcc))
  }
}

object MuSigTweakContext {

  val empty: MuSigTweakContext =
    MuSigTweakContext(FieldElement.one, FieldElement.zero)
}
