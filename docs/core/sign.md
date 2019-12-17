---
id: sign
title: Sign api
---

### The [`Sign` API](org/bitcoins/core/crypto/Sign.scala)

This is the API we define to sign things with. It takes in an arbitrary byte vector and returns a `Future[ECDigitalSignature]`. The reason we incorporate `Future`s here is for extensibility of this API. We would like to provide implementations of this API for hardware devices, which need to be asynchrnous since they may require user input.

From [Sign.scala](../../core/src/main/scala/org/bitcoins/core/crypto/Sign.scala):

```scala mdoc
import scodec.bits._
import org.bitcoins.core.crypto._
import scala.concurrent._
import scala.concurrent.duration._

trait Sign {
  def signFunction: ByteVector => Future[ECDigitalSignature]

  def signFuture(bytes: ByteVector): Future[ECDigitalSignature] =
    signFunction(bytes)

  def sign(bytes: ByteVector): ECDigitalSignature = {
    Await.result(signFuture(bytes), 30.seconds)
  }

  def publicKey: ECPublicKey
}

```

The `ByteVector` that is input to the `signFunction` should be the hash that is output from [`TransactionSignatureSerializer`](/api/org/bitcoins/core/crypto/TransactionSignatureSerializer)'s `hashForSignature` method. Our in-memory [`ECKey`](/api/org/bitcoins/core/crypto/ECKey) types implement the `Sign` API.

If you wanted to implement a new `Sign` api for a hardware wallet, you can easily pass it into the `TxBuilder`/`Signer` classes to allow for you to use those devices to sign with Bitcoin-S.

This API is currently used to sign ordinary transactions with our [`Signer`](/api/org/bitcoins/core/wallet/signer/Signer)s. The `Signer` subtypes (i.e. `P2PKHSigner`) implement the specific functionality needed to produce a valid digital signature for their corresponding script type.


### The [`ExtSign`](../../core/src/main/scala/org/bitcoins/core/crypto/Sign.scala) API.

An [ExtKey](org/bitcoins/core/crypto/ExtKey.scala) is a data structure that can be used to generate more keys from a parent key. For more information look at [hd-keys.md](hd-keys.md)

You can sign with `ExtPrivateKey` the same way you could with a normal `ECPrivateKey`.

```scala mdoc
scala> import org.bitcoins.core.crypto._
import org.bitcoins.core.crypto._

scala> val extPrivKey = ExtPrivateKey(ExtKeyVersion.SegWitMainNetPriv)
extPrivKey: org.bitcoins.core.crypto.ExtPrivateKey = zprvAWgYBBk7JR8Gm5KU7FQ6uiwgBXDSizdmcQnS68WW2quoFrZoCQpJV5jCXniZzuetnPafRUeJmZ6KbZFnWtchgYzEGv47c54Ss2AN8RpYv4C

scala> extPrivKey.sign(DoubleSha256Digest.empty.bytes)
res2: org.bitcoins.core.crypto.ECDigitalSignature = ECDigitalSignature(3045022100f4bf38ef54a51dd110eadefe84ccffa4aabe3ccde387be09e20418f39a1ad29502202f6b3c7dd4aa83ce03014bce970ce7a1ebb2469f0cd4a820c1baa598ef3d106c)

```

With `ExtSign`, you can use `ExtPrivateKey` to sign transactions inside of `TxBuilder` since `UTXOSpendingInfo` takes in `Sign` as a parameter.