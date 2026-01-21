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
  require(
    n == ids.length,
    s"ids length must be equal to u, got ids length: ${ids.length} with n: $n")
  require(
    n == pubshares.length,
    s"pubshare length must be equal to u, got pubshare length: ${pubshares.length} with n: $u")
  require(ids.toSet.size == ids.length, s"All ids must be unique, got: $ids")

  private def pk: ECPublicKey = {
    val pointSum = SecpPoint.sum(pubshares.map(_.toPoint))
    pointSum match {
      case s: SecpPointFinite => s.toPublicKey
      case SecpPointInfinity =>
        throw new IllegalArgumentException(
          s"Computed aggregate public key is point at infinity, invalid pubshares: $pubshares")
    }
  }
  require(
    pk == thresholdPubKey,
    s"Computed threshold pubkey $pk does not match provided threshold pubkey $thresholdPubKey")
}
