package org.bitcoins.core.util.sorted

import org.bitcoins.crypto.SchnorrNonce

/** Represents an ordered set of SchnorrNonces */
case class OrderedNonces(vec: Vector[SchnorrNonce])
    extends SortedVec[SchnorrNonce, SchnorrNonce](vec,
                                                  SortedVec.forOrdered(vec))

object OrderedNonces {

  def apply(single: SchnorrNonce): OrderedNonces = {
    OrderedNonces(Vector(single))
  }
}
