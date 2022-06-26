package org.bitcoins.crypto.musig

import org.bitcoins.crypto.{EvenParity, FieldElement, KeyParity, OddParity}

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
