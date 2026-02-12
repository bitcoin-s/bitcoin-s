package org.bitcoins.crypto.frost

import org.bitcoins.crypto.{ECPublicKey, FieldElement}

case class FrostShareGenResult(
    ids: Vector[Long],
    shares: Vector[FieldElement],
    commitments: Vector[ECPublicKey]) {
  require(ids.length == shares.length)
  require(ids.forall(_ >= 0))
}
