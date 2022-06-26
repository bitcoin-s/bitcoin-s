package org.bitcoins.crypto.musig

import org.bitcoins.crypto.{ECPublicKey, FieldElement}

case class MuSigTweak(tweak: FieldElement, isXOnlyT: Boolean) {
  def point: ECPublicKey = tweak.getPublicKey
}
