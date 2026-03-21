---
id: version-1.9.12-sign
title: Sign API
original_id: sign
---

### The [`Sign` API](/api/org/bitcoins/crypto/Sign)

This is the API we define to sign things with. It takes in an arbitrary byte vector and returns a `Future[ECDigitalSignature]`. The reason we incorporate `Future`s here is for extensibility of this API. We would like to provide implementations of this API for hardware devices, which need to be asynchronous since they may require user input.

From [Sign.scala](/api/org/bitcoins/crypto/Sign):

```scala
import scodec.bits._
import org.bitcoins.crypto._
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

The `ByteVector` that is input to the `signFunction` should be the hash that is output from [`TransactionSignatureSerializer`](/api/org/bitcoins/core/crypto/TransactionSignatureSerializer)'s `hashForSignature` method. Our in-memory [`BaseECKey`](/api/org/bitcoins/crypto/BaseECKey) types implement the `Sign` API.

If you wanted to implement a new `Sign` api for a hardware wallet, you can easily pass it into the `TxBuilder`/`Signer` classes to allow for you to use those devices to sign with Bitcoin-S.

This API is currently used to sign ordinary transactions with our [`Signer`](/api/org/bitcoins/core/wallet/signer/Signer)s. The `Signer` subtypes (i.e. `P2PKHSigner`) implement the specific functionality needed to produce a valid digital signature for their corresponding script type.


### The [`ExtSign`](/api/org/bitcoins/crypto/Sign) API.

An [ExtKey](/api/org/bitcoins/core/crypto/ExtKey) is a data structure that can be used to generate more keys from a parent key. For more information look at [hd-keys.md](../core/hd-keys.md)

You can sign with `ExtPrivateKey` the same way you could with a normal `ECPrivateKey`.

```scala
import org.bitcoins.core.hd._
import org.bitcoins.core.crypto._

val extPrivKey = ExtPrivateKey(ExtKeyVersion.SegWitMainNetPriv)
// extPrivKey: ExtPrivateKey = Masked(ExtPrivateKeyImpl)

extPrivKey.sign(DoubleSha256Digest.empty.bytes)
// res0: ECDigitalSignature = ECDigitalSignature(3044022028eb21fe691b39ae7ebd1f3238304a4bb7c274414792220875c445bdf66e7521022019d453a68d4526ab0b62f33cb0ba9e0e067c135e4cd48be3f5c5d0405cfb7fc6)

val path = BIP32Path(Vector(BIP32Node(0,HardenedType.defaultOpt)))
// path: BIP32Path = m/0'

extPrivKey.sign(DoubleSha256Digest.empty.bytes,path)
// res1: ECDigitalSignature = ECDigitalSignature(304402201de3ec98a1525e5262552c6674750d86040e8319c1716a7fbe1dad95125b9dfc02201c9c585a303dbc70a5dfec8563928db057adec45ef3266d0541e72a0afb15493)
```

With `ExtSign`, you can use `ExtPrivateKey` to sign transactions inside of `TxBuilder` since `UTXOSpendingInfo` takes in `Sign` as a parameter. 

You can also provide a `path` to use to derive a child `ExtPrivateKey`, and then sign with that child private key
