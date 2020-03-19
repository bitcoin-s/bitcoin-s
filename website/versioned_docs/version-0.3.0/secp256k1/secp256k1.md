---
id: version-0.3.0-secp256k1
title: Secp256k1
original_id: secp256k1
---

[Libsecp256k1](https://github.com/bitcoin-core/secp256k1) is used to preform cryptographic operations on the secp256k1 curve.
This is the curve that bitcoin uses. There is a _signficant_ speedup when using this library compared to java crypto libraries
like bouncy castle.

In bitcoin-s, we support native binaries for libsecp256k1

1. [linux 32 bit](../../secp256k1jni/natives/linux_32)
2. [linux 64 bit](../../secp256k1jni/natives/linux_64)
3. [mac osx 64 bit](../../secp256k1jni/natives/osx_64)
4. [windows 64 bit](../../secp256k1jni/natives/windows_64)

Bitcoin-s uses a zero dependency library called [`native-lib-loader`](https://github.com/scijava/native-lib-loader). 
That does the appropriate loading of the library onto your classpath to be accessed.

#### Using libsecp256k1

To tell if you have access to libsecp256k1 you can do the following


```scala
val isEnabled = org.bitcoin.Secp256k1Context.isEnabled()

println(s"Secp256k1Context.isEnabled=${isEnabled}")
```

If libsecp256k1 is enabled, you can use [NativeSecp256k1](../../secp256k1jni/src/main/java/org/bitcoin/NativeSecp256k1.java)
with static method defined in the class.


```scala
val privKey = ECPrivateKey.freshPrivateKey
val pubKey = privKey.publicKey
val dataToSign = DoubleSha256Digest.empty

val signature = NativeSecp256k1.sign(dataToSign.bytes.toArray, privKey.bytes.toArray)
    
val verify = NativeSecp256k1.verify(dataToSign.bytes.toArray, signature, pubKey.bytes.toArray)

println(s"Verified with NativeSecp256k1 signature=${verify}")

//you can also just directly sign with the ECKey interface:
val signature2 = privKey.sign(dataToSign)

val verified2 = pubKey.verify(dataToSign, signature2)

println(s"Verified with NativeSecp256k1 again=${verified2}")
```

### When libsecp256k1 isn't available, or you want to turn it off

There are two reasons you wouldn't want to use libsecp256k1 

1. You don't trust the pre-compiled binaries we are using
2. Your OS/arch is not supported

There are two ways you can circumvent libsecp256k1

1. Set `DISABLE_SECP256K1=true` in your environment variables. This will force `Secp256k1Context.isEnabled()` to return false
2. Call Bouncy castle methods in `ECKey`. 

Here is an example of calling bouncy castle methods in `ECKey`

```scala
val privKey = ECPrivateKey.freshPrivateKey
// privKey: ECPrivateKey = Masked(ECPrivateKeyImpl)
val publicKey = privKey.publicKeyWithBouncyCastle
// publicKey: ECPublicKey = ECPublicKey(03b12dbe31d79acc699af9ea0c741ab1e98d821a885ad8690cfc2f9921decc771b)
val dataToSign = DoubleSha256Digest.empty
// dataToSign: DoubleSha256Digest = DoubleSha256Digest(0000000000000000000000000000000000000000000000000000000000000000)

val signature = privKey.signWithBouncyCastle(dataToSign.bytes)
// signature: ECDigitalSignature = ECDigitalSignature(30440220042bb6ce44f919a759e341f4f87e6ccd074253161e50d1d84668da68fbfb3f2a022036c759496f425b24446c79240dcc8fc1fe3c5c4ae2619fcb960efae98e37df26)

val verified = publicKey.verifyWithBouncyCastle(dataToSign.bytes, signature)
// verified: Boolean = true

println(s"Verified with bouncy castle=${verified}")
// Verified with bouncy castle=true
```

