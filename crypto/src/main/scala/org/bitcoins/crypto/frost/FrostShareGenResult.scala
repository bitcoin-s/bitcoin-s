package org.bitcoins.crypto.frost

import org.bitcoins.crypto.{ECPublicKey, FieldElement}

case class FrostShareGenResult(
    ids: Vector[Long],
    shares: Vector[FieldElement],
    commitments: Vector[ECPublicKey]) {
  require(ids.length == shares.length)
  require(ids.forall(_ >= 0))
  require(commitments.length <= ids.length)
  require(shares.length >= commitments.length,
          s"Must have more commitments than shares")

  def threshold: Int = commitments.length

  def toSigningContext(
      participantIds: Vector[Long],
      participantPubShares: Vector[ECPublicKey]): FrostSigningContext = {
    require(
      participantIds.length == participantPubShares.length,
      s"Must have the same number of participant ids and pubshares, got ${participantIds.length} participant ids and ${participantPubShares.length} pubshares"
    )
    require(participantIds.forall(id => ids.contains(id)))
    val allPubShares = shares.map(FieldElement.computePoint)
    require(
      participantPubShares.forall(pk => allPubShares.contains(pk)),
      s"All participant pubshares must be in the commitments, got participant pubshares: $participantPubShares with commitments: $allPubShares"
    )
    FrostSigningContext.fromShareGen(this, participantIds, participantPubShares)
  }
}
