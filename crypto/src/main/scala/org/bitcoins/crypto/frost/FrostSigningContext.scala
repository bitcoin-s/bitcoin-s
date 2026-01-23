package org.bitcoins.crypto.frost

import org.bitcoins.crypto.{
  ECPublicKey,
  SecpPoint,
  SecpPointFinite,
  SecpPointInfinity
}

case class FrostSigningContext(
    n: Long,
    t: Long,
    u: Long,
    ids: Vector[Int],
    pubshares: Vector[ECPublicKey],
    thresholdPubKey: ECPublicKey) {
  require(n >= 2 && n < 4294967296L,
          s"n must be in the range [2, 2^32 - 1], got: $n")
  require(1 <= t && t <= n,
          s"t must be in the range [1, n], got: $t with n: $n")
  require(u >= t && u <= n,
          s"u must be in the range [t, n], got: $u with t: $t and n: $n")
  // TODO: Come back and look at these invariants, shouldn't they be equal?
  require(
    u <= ids.length,
    s"Must have enough ids to satisfy u, ids length: ${ids.length} with u: $u")
  require(
    u <= pubshares.length,
    s"Must have enough pubshares to satisfy u, pubshares length: ${pubshares.length} with u: $u")
  require(ids.toSet.size == ids.length, s"All ids must be unique, got: $ids")

  private def pk: ECPublicKey = {
    var q: SecpPoint = SecpPointInfinity
    pubshares.zipWithIndex.foreach { case (p, idx) =>
      val myId = ids(idx)
      val interpolation =
        FrostUtil.deriveInterpolatingValue(ids = ids, myId = myId)
      val x = p.toPoint.multiply(interpolation)
      q = q.add(x)
    }
    q match {
      case SecpPointInfinity =>
        throw new IllegalArgumentException(
          s"Computed threshold pubkey is point at infinity")
      case p: SecpPointFinite => p.toPublicKey
    }
  }
  require(
    pk == thresholdPubKey,
    s"Computed threshold pubkey $pk does not match provided threshold pubkey $thresholdPubKey")
}
