package org.bitcoins.crypto.musig

import org.bitcoins.crypto.{FieldElement, SecpPointFinite}

case class MuSigSessionValues(
    tweakContext: MuSigTweakContext,
    b: FieldElement,
    R: SecpPointFinite,
    e: FieldElement) {
  def Q: SecpPointFinite = tweakContext.Q

  def gacc: ParityMultiplier = tweakContext.parityAcc

  def tacc: FieldElement = tweakContext.tweakAcc
}
