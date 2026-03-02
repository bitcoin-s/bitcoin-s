---
id: frost
title: FROST
---

Bitcoin-S now has support for [FROST](https://eprint.iacr.org/2020/852) (Flexible Round-Optimized Schnorr Threshold Signatures).

FROST is a threshold signature scheme where a group of `n` participants can collectively sign a message, but only a subset of at least `t` participants (the threshold) is needed to produce a valid signature. The resulting signature is a standard [BIP-340 Schnorr signature](https://github.com/bitcoin/bips/blob/master/bip-0340.mediawiki) that is indistinguishable from a single-party signature.

This module contains classes and utility functions for all FROST computations, including distributed key generation, nonce generation, partial signing, and signature aggregation.

## Key Generation

Key generation in FROST uses a trusted dealer model with Verifiable Secret Sharing (VSS). The dealer generates secret shares for all `n` participants such that any `t` of them can reconstruct the signing key.

The key generation functions are:

* `FrostUtil.generateShares`
  * Given a seed, threshold `t`, and total number of participants `n`, returns a `FrostShareGenResult` containing secret shares, VSS commitments, and participant IDs. Each participant receives their secret share (kept private) and the public VSS commitments (shared with all participants).
* `FrostUtil.vssVerify`
  * Allows each participant to verify that their secret share is consistent with the public VSS commitments, ensuring the dealer did not give them a malformed share.

## Signing

The FROST signing protocol is a two-round protocol:

**Round 1: Nonce Generation**

* `FrostUtil.nonceGen`
  * Each participant generates a pair of secret/public nonces for the signing session. Takes optional context inputs (secret share, public share, threshold public key, message, extra data) to improve security. Returns a `(FrostNoncePriv, FrostNoncePub)` pair. The public nonce is broadcast to all participants.

**Round 2: Signing**

* `FrostUtil.aggregateNonces`
  * Given the public nonces from all participants and their IDs, computes the aggregate nonce `(R1, R2)` used in the session.
* `FrostUtil.sign`
  * Each participant computes a partial signature using their secret nonce, secret share, participant ID, and the session context. Returns a partial signature scalar.
* `FrostUtil.partialSigVerify`
  * Verifies a single partial signature from a participant against their public share and public nonce.
* `FrostUtil.partialSigAgg`
  * Aggregates all partial signatures into a single valid `SchnorrDigitalSignature`. Any party can perform this aggregation.

Note that no new function is required for aggregate verification: `SchnorrPublicKey`'s `verify` function (via the threshold public key) is used directly.

The following code shows a 2-of-3 FROST threshold signing execution:

```scala mdoc:invisible
import org.bitcoins.crypto._
import org.bitcoins.crypto.frost._
import scodec.bits.ByteVector
import scala.util.Random

val seed = ECPrivateKey.freshPrivateKey
val threshold = 2
val numShares = 3
val result = FrostUtil.generateShares(seed, threshold = threshold, numShares = numShares)

val message = CryptoUtil.sha256(ByteVector.view("Hello World!".getBytes())).bytes

val participantIds: Vector[Long] = Vector(1, 2)
val secShares = participantIds.map(id => result.shares((id - 1).toInt))
val participantPubShares = secShares.map(_.toPoint.toPublicKey)
```

```scala mdoc:compile-only
// === Round 1: Nonce Generation ===
// Each participant generates a fresh nonce pair for this signing session
val nonceData = secShares.zip(participantPubShares).map { case (secShare, pubShare) =>
  val rand = ByteVector.view(Random.nextBytes(32))
  FrostUtil.nonceGen(
    rand = rand,
    secshare = Some(secShare),
    pubshare = Some(pubShare),
    threshold_pk = None,
    message = Some(message),
    extra_in = None
  )
}
val secNonces = nonceData.map(_._1) // kept secret by each participant
val pubNonces = nonceData.map(_._2) // broadcast to all participants

// === Round 2: Signing ===
// Any party can aggregate the public nonces
val aggNonce = FrostUtil.aggregateNonces(pubnonces = pubNonces,
                                         participantIdentifiers = participantIds)

// Build the signing context from the share generation result
val signingContext = result.toSigningContext(participantIds, participantPubShares)

// Build the session context (no tweaks in this example)
val sessionCtx = FrostSessionContext(
  signingContext = signingContext,
  aggNonce = aggNonce,
  tweaks = Vector.empty,
  isXOnly = Vector.empty,
  message = message
)

// Each participant computes their partial signature
val partialSigs = secShares.zipWithIndex.map { case (secShare, idx) =>
  val signerId = participantIds(idx)
  FrostUtil.sign(secNonce = secNonces(idx),
                 secShare = secShare,
                 signerId = signerId,
                 sessionContext = sessionCtx)
}

// Optionally verify each partial signature before aggregating
partialSigs.zipWithIndex.foreach { case (pSig, idx) =>
  val signerId = participantIds(idx)
  require(
    FrostUtil.partialSigVerify(
      partialSig = pSig,
      pubnonces = pubNonces,
      signersContext = signingContext,
      tweaks = Vector.empty,
      isXonlyT = Vector.empty,
      message = message,
      signerId = signerId
    ),
    s"Partial signature invalid for participant $signerId"
  )
}

// Aggregate the partial signatures into a final Schnorr signature
val sig = FrostUtil.partialSigAgg(partialSigs, participantIds, sessionCtx)

// Verify the final signature against the threshold public key (standard BIP-340)
val thresholdPubKey = signingContext.thresholdPubKey.toXOnly
require(thresholdPubKey.verify(message, sig))
```

## Tweaks (e.g., Taproot)

FROST supports applying scalar tweaks to the threshold public key, which is required for [BIP-341 Taproot](https://github.com/bitcoin/bips/blob/master/bip-0341.mediawiki) key path spending. Tweaks are passed to the `FrostSessionContext` and are automatically accounted for during signing and aggregation.

```scala mdoc:invisible
import org.bitcoins.crypto._
import org.bitcoins.crypto.frost._
import scodec.bits.ByteVector
import scala.util.Random

val seed2 = ECPrivateKey.freshPrivateKey
val result2 = FrostUtil.generateShares(seed2, threshold = 2, numShares = 3)
val message2 = CryptoUtil.sha256(ByteVector.view("Taproot tx".getBytes())).bytes
val participantIds2: Vector[Long] = Vector(1, 2)
val secShares2 = participantIds2.map(id => result2.shares((id - 1).toInt))
val participantPubShares2 = secShares2.map(_.toPoint.toPublicKey)
val signingContext2 = result2.toSigningContext(participantIds2, participantPubShares2)
val nonceData2 = secShares2.zip(participantPubShares2).map { case (sec, pub) =>
  FrostUtil.nonceGen(ByteVector.view(Random.nextBytes(32)), Some(sec), Some(pub), None, Some(message2), None)
}
val secNonces2 = nonceData2.map(_._1)
val pubNonces2 = nonceData2.map(_._2)
val aggNonce2 = FrostUtil.aggregateNonces(pubNonces2, participantIds2)
```

```scala mdoc:compile-only
// A Taproot internal key tweak (x-only)
val tapTweak = FieldElement.one // use actual tweak in practice

val sessionCtxWithTweak = FrostSessionContext(
  signingContext = signingContext2,
  aggNonce = aggNonce2,
  tweaks = Vector(tapTweak),
  isXOnly = Vector(true),
  message = message2
)

val partialSigs2 = secShares2.zipWithIndex.map { case (secShare, idx) =>
  FrostUtil.sign(secNonce = secNonces2(idx),
                 secShare = secShare,
                 signerId = participantIds2(idx),
                 sessionContext = sessionCtxWithTweak)
}

val sig2 = FrostUtil.partialSigAgg(partialSigs2, participantIds2, sessionCtxWithTweak)

// Verify against the tweaked key
val tweakedKey = FrostTweakContext.calculateTweakedKey(
  signingContext2.thresholdPubKey,
  Vector(tapTweak),
  Vector(true)
)
require(tweakedKey.toXOnly.verify(message2, sig2))
```
