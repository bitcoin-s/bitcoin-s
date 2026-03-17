---
id: adaptor-signatures
title: Adaptor Signatures
---

Bitcoin-S now has support
for [ECDSA Adaptor Signatures](https://github.com/discreetlogcontracts/dlcspecs/blob/03bf7095c2016e1ce9c9fb612920872d4456f179/ECDSA-adaptor.md).

There are four relevant functions to adaptor signatures:

* `sign` (aka encrypt)
    * This function belongs to `ECPrivateKey` and creates an adaptor signature given a message (`ByteVector`) and an
      adaptor point (`ECPublicKey`).
* `verify`
    * Verifies an adaptor signature given the signing public key, the message and the adaptor point.
* `complete` (aka decrypt)
    * This function belongs to `ECPrivateKey` and computes a valid ECDSA signature given a valid adaptor signature whose
      adaptor point is this private key's public key.
* `extract` (aka recover)
    * This function belongs to `ECPublicKey` and computes the adaptor secret (private key to this public key) given a
      valid adaptor signature for this adaptor point, and the valid ECDSA signature computed using `complete`.

The following code shows each function to do with adaptor signature usage:

```scala mdoc:invisible
import org.bitcoins.crypto.ECPrivateKey

val privKey = ECPrivateKey.freshPrivateKey
val pubKey = privKey.publicKey
val adaptorSecret = ECPrivateKey.freshPrivateKey
val adaptorPoint = adaptorSecret.publicKey
val msg = scodec.bits.ByteVector.fromValidHex("010101010101")
```

```scala mdoc:compile-only
// Alice generates an adaptor signature using her private key and the adaptor point
val adaptorSig = privKey.adaptorSign(adaptorPoint, msg)

// Bob verifies this adaptor signature using Alice's public key and the adaptor point
require(pubKey.adaptorVerify(msg, adaptorPoint, adaptorSig))

// Bob computes a valid ECDSA signature using the adaptorSignature, which he knows
val sig = adaptorSecret.completeAdaptorSignature(adaptorSig)

// Anyone can validate this signature
require(pubKey.verify(msg, sig))

// Alice can compute the adaptor secret from the signatures
val secret = adaptorPoint.extractAdaptorSecret(adaptorSig, sig)
require(secret == adaptorSecret)
```

## Schnorr Adaptor Signatures

Bitcoin-S also supports Schnorr Adaptor Signatures which are BIP340 compatible.

The functions are located in `AdaptorUtil`:

* `schnorrAdaptorSign`
    * Creates an adaptor signature given a private key, an adaptor point (`ECPublicKey`), and a message (`ByteVector`).
* `schnorrAdaptorVerify`
    * Verifies an adaptor signature given the signing public key (`XOnlyPubKey`), the message and the adaptor point.
* `schnorrAdaptorComplete`
    * Computes a valid Schnorr signature given a valid adaptor signature and the adaptor secret.
* `schnorrExtractSecret`
    * Computes the adaptor secret given a valid adaptor signature and the valid Schnorr signature computed using
      `schnorrAdaptorComplete`.

```scala mdoc:compile-only
import org.bitcoins.crypto._

val privKey = ECPrivateKey.freshPrivateKey
val pubKey = privKey.schnorrPublicKey
val adaptorSecret = ECPrivateKey.freshPrivateKey
val adaptorPoint = adaptorSecret.publicKey
val msg = scodec.bits.ByteVector.fromValidHex("010101010101")
val auxRand = scodec.bits.ByteVector.fill(32)(0x55)

// Alice generates an adaptor signature using her private key and the adaptor point
val adaptorSig = AdaptorUtil.schnorrAdaptorSign(privKey, adaptorPoint, msg, Some(auxRand))

// Bob verifies this adaptor signature using Alice's public key from the schnorr public key
// Note: verify uses XOnlyPubKey
require(AdaptorUtil.schnorrAdaptorVerify(adaptorSig, pubKey.toXOnly, msg, adaptorPoint))

// Bob computes a valid Schnorr signature using the adaptorSignature, which he knows
val sig = AdaptorUtil.schnorrAdaptorComplete(adaptorSecret, adaptorSig)

// Anyone can validate this signature
require(pubKey.verify(msg, sig))

// Alice can compute the adaptor secret from the signatures
val secret = AdaptorUtil.schnorrExtractSecret(sig, adaptorSig, adaptorPoint)
require(secret == adaptorSecret)
```
