package org.bitcoins.crypto.musig

import org.bitcoins.crypto.{ECPublicKey, FieldElement}

/** Used to tweak a MuSig aggregate public key, as defined here
  * https://github.com/jonasnick/bips/blob/musig2/bip-musig2.mediawiki#tweaking-definition
  */
case class MuSigTweak(tweak: FieldElement, isXOnlyT: Boolean) {
  def point: ECPublicKey = tweak.getPublicKey
}
