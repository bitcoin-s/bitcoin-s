package org.bitcoins.crypto.frost

import org.bitcoins.crypto.ECPublicKey

case class FrostSigningContext(
    n: Long,
    t: Long,
    participantIds: Vector[Long],
    pubshares: Vector[ECPublicKey],
    thresholdPubKey: ECPublicKey) {
  require(n >= 2 && n < 4294967296L,
          s"n must be in the range [2, 2^32 - 1], got: $n")
  require(1 <= t && t <= n,
          s"t must be in the range [1, n], got: $t with n: $n")

  def u: Int = participantIds.length

  require(u >= t && u <= n,
          s"u must be in the range [t, n], got: $u with t: $t and n: $n")
  // TODO: Come back and look at these invariants, shouldn't they be equal?
  require(
    participantIds.length == pubshares.length,
    s"Must have the same number of ids and pubshares, ids length: ${participantIds.length} with pubshares length: ${pubshares.length}"
  )
  require(participantIds.toSet.size == participantIds.length,
          s"All ids must be unique, got: $participantIds")
  require(
    participantIds.forall(id => id >= 0 && id <= n),
    s"All ids must be in the range [0, n], got: $participantIds with n: $n")

  private def pk: ECPublicKey = {
    FrostUtil.computeThresholdPubKey(pubshares, participantIds)
  }

  require(
    pk == thresholdPubKey,
    s"Computed threshold pubkey $pk does not match provided threshold pubkey $thresholdPubKey")
}

object FrostSigningContext {
  def fromShareGen(
      result: FrostShareGenResult,
      participantIds: Vector[Long],
      pubshares: Vector[ECPublicKey]): FrostSigningContext = {
    FrostSigningContext(
      n = result.shares.length,
      t = result.threshold,
      participantIds = participantIds,
      pubshares = pubshares,
      thresholdPubKey =
        FrostUtil.computeThresholdPubKey(pubshares, participantIds)
    )
  }
}
