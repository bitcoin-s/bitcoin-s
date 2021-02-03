---
id: version-0.5.0-crypto-intro
title: Crypto Module
original_id: crypto-intro
---

The `crypto` module contains the base cryptographic functionality for Bitcoin-s. Specifically, this includes the fundamental cryptographic data structures and functions such as keys, hashing encryption and signing. This module does not include secondary cryptographic types and functions such as those pertaining to BIP 32 or 39, nor does it include signature code specific to Bitcoin transactions. For all of these things, see the [core module](../core/core-intro.md). It is very important to keep this code well tested, and we also try to minimize the dependencies of this module.

## Dealing with Array[Byte]

The crypto module contains a trait called `NetworkElement` which is extended by all data structures with byte serializations. This allows for a uniform interface when dealing with serialization and deserialization which can be very important when constructing hashes. Additionally, the `Factory` trait can be extended by companion objects of `NetworkElement`s to allow for out-of-the-box constructors from String hex or bytes in little or big endian given one constructor. The `CryptoUtil` object provides easy access to hash functions and `HashDigest` wraps the results in nice types. Lastly, this module also contains a `BytesUtil` for dealing with raw bytes and raw hex.

## AES Encryption

The crypto module contains support for AES using the object `AesCrypt` and its companion data structures.

## Elliptic Curve Keys and Functions

`ECKey.scala` contains types `ECPublicKey` and `ECPrivateKey` which represent private and public keys for the secp256k1 curve (used by Bitcoin).
There also exists `SchnorrPublicKey` and `SchnorrNonce` that can be used for doing BIP 340 compatible Schnorr signatures. 

These keys also implement functions for using them to create and verify `ECDigitalSignature`s, `SchnorrDigitalSignatures`, and `ECPrivateKey` implements the [`Sign` interface](sign.md). Note that all sensitive information (such as private keys) extends the `MaskToString` interface which overrides the `toString` method to mask the actual result unless `toStringSensitive` is explicitly called.
This is done to avoid accidentally logging keys.

Utility functions for signatures (such as checking for or flipping to low s) can be found in `DERSignatureUtil`.

Lastly, Bitcoin-S uses Java Native Interface (JNI) bindings to the Bitcoin library secp256k1 to execute cryptographic functions by default. See [this doc](../secp256k1/secp256k1.md) for more details. However, fall-back implementations of all cryptographic functions supported and used exist using the bouncy castle library in `BouncyCastleUtil`.

## Basic Examples


### ECDSA Example

```scala
// Randomly generate new key
val privateKey = ECPrivateKey.freshPrivateKey

// Construct private key from hex
val privateKeyFixed = ECPrivateKey("6846a082d76e7c34cd2deddc6ef3d4cb3220e6c72c7c9ec03408d60ed976837c")

// Compute public key from private key
val publicKey = privateKey.publicKey

// Take SHA256 hash
val hash = CryptoUtil.sha256(ByteVector("Hello".getBytes()))

// Sign and verify signature
val sig = privateKey.sign(hash)
val validSig = publicKey.verify(hash, sig)
```

### Schnorr Example

```scala
// Randomly generate new key
val privateKey = ECPrivateKey.freshPrivateKey

// Construct private key from hex
val privateKeyFixed = ECPrivateKey("6846a082d76e7c34cd2deddc6ef3d4cb3220e6c72c7c9ec03408d60ed976837c")

// Compute schnorr public key from private key
val publicKey = privateKey.schnorrPublicKey

// Take SHA256 hash
val hash = CryptoUtil.sha256(ByteVector("Hello".getBytes()))

// Sign and verify signature
val sig = privateKey.schnorrSign(hash.bytes)
val validSig = publicKey.verify(hash.bytes, sig)
```