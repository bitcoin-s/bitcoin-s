package org.bitcoins.crypto.musig

import org.bitcoins.crypto.{EvenParity, FieldElement, KeyParity, OddParity}

/** The data required to apply the net tweak during
  * MuSig signature aggregation
  */
case class MuSigTweakData(
    context: MuSigTweakContext,
    aggPubKeyParity: KeyParity,
    e: FieldElement) {

  def additiveTweak: FieldElement = {
    val g = aggPubKeyParity match {
      case EvenParity => FieldElement.one
      case OddParity  => FieldElement.orderMinusOne
    }

    e.multiply(g).multiply(context.tweakAcc)
  }
}
