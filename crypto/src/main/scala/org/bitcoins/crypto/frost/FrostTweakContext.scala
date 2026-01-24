package org.bitcoins.crypto.frost
import org.bitcoins.crypto.{ECPublicKey, FieldElement, SecpPointFinite}

case class FrostTweakContext(
    q: SecpPointFinite,
    tacc: FieldElement,
    gacc: FieldElement) {
  require(gacc == FieldElement.one || gacc == FieldElement.orderMinusOne,
          s"gacc must be either 1 or -1, got: $gacc")
}

object FrostTweakContext {

  def apply(key: ECPublicKey) : FrostTweakContext = {
    val point = key.toPoint match {
      case p: SecpPointFinite => p
      case _ =>
        throw new IllegalArgumentException(
          s"Public key must not be point at infinity")
    }
    FrostTweakContext(point, tacc = FieldElement.zero, gacc = FieldElement.one)
  }
}
