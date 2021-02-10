---
id: version-0.5.0-secp256k1
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

### Using libsecp256k1

To tell if you have access to libsecp256k1 you can do the following


```scala
val isEnabled = org.bitcoin.Secp256k1Context.isEnabled()

println(s"Secp256k1Context.isEnabled=${isEnabled}")
```

If libsecp256k1 is enabled, you can use [NativeSecp256k1](/api/org/bitcoin/NativeSecp256k1)
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

1. Set `DISABLE_SECP256K1=true` in your environment variables. This will force `CryptoContext.default` to return false which will make Bitcoin-S act like `Secp256k1Context.isEnabled()` has returned false.
2. Call Bouncy castle methods in `ECKey`. 

Here is an example of calling bouncy castle methods in `ECKey`

```scala
val privKey = ECPrivateKey.freshPrivateKey
// privKey: ECPrivateKey = Masked(ECPrivateKeyImpl)
val publicKey = privKey.publicKeyWithBouncyCastle
// publicKey: ECPublicKey = ECPublicKey(0322b9a4188a8b9541d8d4e6be9e418ff7ab44d8655cb63145703939e6a348ef43)
val dataToSign = DoubleSha256Digest.empty
// dataToSign: DoubleSha256Digest = DoubleSha256Digest(0000000000000000000000000000000000000000000000000000000000000000)

val signature = privKey.signWithBouncyCastle(dataToSign.bytes)
// signature: ECDigitalSignature = ECDigitalSignature(3045022100b24cc0a8e9f778899db470dce18b7902e87881343484fc7c0c32058f8a73ddff022027d0600df4a69a12ad03f5a23736be58fcdd4ca2c14b202ef9a643d1632fde6b)

val verified = publicKey.verifyWithBouncyCastle(dataToSign.bytes, signature)
// verified: Boolean = true

println(s"Verified with bouncy castle=${verified}")
// Verified with bouncy castle=true
```

### Building libsecp256k1

[See instructions here](add-to-jni.md#adding-to-bitcoin-s)
