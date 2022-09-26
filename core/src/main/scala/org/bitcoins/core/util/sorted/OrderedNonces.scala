package org.bitcoins.core.util.sorted

import org.bitcoins.crypto.SchnorrNonce

/** Represents an ordered set of SchnorrNonces */
case class OrderedNonces(private val vec: Vector[SchnorrNonce])
    extends SortedVec[SchnorrNonce, SchnorrNonce](
      vec,
      org.bitcoins.core.nonceOrdering)

object OrderedNonces {

  def apply(single: SchnorrNonce): OrderedNonces = {
    OrderedNonces(Vector(single))
  }

  def fromUnsorted(vec: Vector[SchnorrNonce]): OrderedNonces = {
    val sorted = vec.sorted(org.bitcoins.core.nonceOrdering)
    OrderedNonces(sorted)
  }
}
