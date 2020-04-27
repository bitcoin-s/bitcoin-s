---
id: version-0.3.0-sign
title: Sign API
original_id: sign
---

### The [`Sign` API](org/bitcoins/core/crypto/Sign.scala)

This is the API we define to sign things with. It takes in an arbitrary byte vector and returns a `Future[ECDigitalSignature]`. The reason we incorporate `Future`s here is for extensibility of this API. We would like to provide implementations of this API for hardware devices, which need to be asynchrnous since they may require user input.

From [Sign.scala](../../core/src/main/scala/org/bitcoins/core/crypto/Sign.scala):

```scala
import org.bitcoins.core.crypto._
import org.bitcoins.crypto.ECDigitalSignature

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

```scala
import org.bitcoins.core.hd._
import org.bitcoins.core.crypto._

val extPrivKey = ExtPrivateKey(ExtKeyVersion.SegWitMainNetPriv)
// extPrivKey: ExtPrivateKey = Masked(ExtPrivateKeyImpl)

extPrivKey.sign(DoubleSha256Digest.empty.bytes)
// res0: ECDigitalSignature = ECDigitalSignature(3044022002ef6ab03b3f0bc86bfea1f149c2730aa9cec84a456c6517ea9815878fa7e42302207159fe6378a1b47a4479aa534eb5c09f2ff78993d28fa6f10f5d9dc9946dcd74)

val path = BIP32Path(Vector(BIP32Node(0,false)))
// path: BIP32Path = m/0

extPrivKey.sign(DoubleSha256Digest.empty.bytes,path)
// res1: ECDigitalSignature = ECDigitalSignature(304502210083e44bafdee66ebf6bae54e8b19c5fb5b6457a72bcb255d67b7ffec5654afa9b0220061981821b0a538e5ec3b033322778cb25190306d4ed054bcba66098c771cafb)
```

With `ExtSign`, you can use `ExtPrivateKey` to sign transactions inside of `TxBuilder` since `UTXOSpendingInfo` takes in `Sign` as a parameter. 

You can also provide a `path` to use to derive a child `ExtPrivateKey`, and then sign with that child private key