package org.bitcoins.core.util.sorted

import org.bitcoins.crypto.SchnorrDigitalSignature

case class OrderedSchnorrSignatures(
    private val vec: Vector[SchnorrDigitalSignature])
    extends SortedVec[SchnorrDigitalSignature, SchnorrDigitalSignature](
      vec,
      org.bitcoins.core.schnorrSignatureOrdering)

object OrderedSchnorrSignatures {

  def apply(sig: SchnorrDigitalSignature): OrderedSchnorrSignatures = {
    OrderedSchnorrSignatures(Vector(sig))
  }

  def fromUnsorted(
      vec: Vector[SchnorrDigitalSignature]): OrderedSchnorrSignatures = {
    val sorted = vec.sorted(org.bitcoins.core.schnorrSignatureOrdering)
    OrderedSchnorrSignatures(sorted)
  }
}
