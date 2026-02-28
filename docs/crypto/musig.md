---
id: musig
title: MuSig
---

Bitcoin-S now has support for [MuSig](https://github.com/jonasnick/bips/blob/musig2/bip-musig2.mediawiki).

This module contains classes representing public `KeySet`s, MuSig nonces, and MuSig aggregate key tweaks, as well as utility functions for all MuSig computations.

The functions for aggregating key data are:

* `aggPubKey`
  * This is a member of `KeySet` and returns the aggregate public key for this set of signers, including the tweaks provided. In most uses, a subsequent call to `schnorrPublicKey` is required for Bitcoin applications.
* `MuSigUtil.aggregateNonces`
  * Given a `Vector[MuSigNoncePub]` of the signer's nonces, returns the aggregate `MuSigNoncePub`. This aggregation can be done before the message, or even the `KeySet` is known.

The functions for signing and verification are:

* `MuSigUtil.sign`
  * This function generates a MuSig partial signature using a private key and `MuSigNoncePriv`. This consists of a pair `(R, s)` where `R` is the aggregate nonce key (same for all signers) and `s` is the actual partial signature that needs to be shared.
* `MuSigUtil.partialSigVerify`
  * This function validates a single partial signature against that signer's public key and `MuSigNoncePub`.
* `MuSigUtil.partialSigAgg`
  * This function aggregates all of the `s` values into a single valid `SchnorrDigitalSignature` (using the aggregate nonce key `R`).

Note that no new function is required for aggregate verification as `SchnorrPublicKey`'s `verify` function is to be used.

Lastly, it should be mentioned that `MuSigNoncePriv`s must be constructed using either `MuSigUtil.nonceGen`.

The following code shows a two-party MuSig execution:

```scala mdoc:invisible
import org.bitcoins.crypto.ECPrivateKey
import org.bitcoins.crypto.musig.*

val msg = scodec.bits.ByteVector.fill(32)(7)
val tweaks = Vector.empty[MuSigTweak]
```

```scala mdoc:compile-only
val alicePrivKey = ECPrivateKey.freshPrivateKey
val alicePubKey = alicePrivKey.publicKey
// Alice and Bob generate and exchange nonce data (new nonces for every sig)
val aliceNoncePriv = MuSigUtil.nonceGen(alicePubKey)
val aliceNonce = aliceNoncePriv.toNoncePub // Alice sends this to Bob

val bobPrivKey = ECPrivateKey.freshPrivateKey
val bobPubKey = bobPrivKey.publicKey
val bobNoncePriv = MuSigUtil.nonceGen(bobPubKey)
val bobNonce = bobNoncePriv.toNoncePub // Bob sends this to Alice

// The aggregate musig nonce can be computed from Alice's and Bob's
val aggMuSigNonce = MuSigUtil.aggregateNonces(Vector(aliceNonce, bobNonce))

// Any party can (non-interactively) compute the aggregate public key
val pubKeys = Vector(alicePubKey, bobPubKey)
val keySet = KeySet(pubKeys, tweaks) // This is where you put MuSigTweaks
val aggPubKey = keySet.aggPubKey.schnorrPublicKey

// Alice generates a partial signature for the message
val aliceSig = 
    MuSigUtil.sign(aliceNoncePriv, aggMuSigNonce, alicePrivKey, msg, keySet)
// Bob generates a partial signature for the message
val bobSig =
    MuSigUtil.sign(bobNoncePriv, aggMuSigNonce, bobPrivKey, msg, keySet)

// Alice and Bob exchange and verify each other's sigs (s values)
require(
        MuSigUtil.partialSigVerify(aliceSig,
                                   aliceNonce,
                                   aggMuSigNonce,
                                   alicePubKey,
                                   keySet,
                                   msg))
require(
        MuSigUtil.partialSigVerify(bobSig,
                                   bobNonce,
                                   aggMuSigNonce,
                                   bobPubKey,
                                   keySet,
                                   msg))

// In the case that the aggregator is not Alice or Bob, R can be computed as follows
val ctx = MuSigSessionContext(aggMuSigNonce, keySet, msg)

// Finally, any party can aggregate the partial signatures
val sig = MuSigUtil.partialSigAgg(Vector(aliceSig, bobSig), ctx)

// This signature can now be validated as a normal BIP340 Schnorr signature
require(aggPubKey.verify(msg, sig))
```

