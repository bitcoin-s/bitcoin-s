---
id: version-v0.4-sign
title: Sign API
original_id: sign
---

### The [`Sign` API](org/bitcoins/core/crypto/Sign.scala)

This is the API we define to sign things with. It takes in an arbitrary byte vector and returns a `Future[ECDigitalSignature]`. The reason we incorporate `Future`s here is for extensibility of this API. We would like to provide implementations of this API for hardware devices, which need to be asynchrnous since they may require user input.

From [Sign.scala](../../core/src/main/scala/org/bitcoins/core/crypto/Sign.scala):

```scala
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

```scala
import org.bitcoins.core.hd._
import org.bitcoins.core.crypto._

val extPrivKey = ExtPrivateKey(ExtKeyVersion.SegWitMainNetPriv)
// extPrivKey: ExtPrivateKey = Masked(ExtPrivateKeyImpl)

extPrivKey.sign(DoubleSha256Digest.empty.bytes)
// res0: ECDigitalSignature = ECDigitalSignature(3045022100cfeda1a9e7ae6fbd4a608b2da83ce84ff5a58d3ce44f32cf8e6db53b34d66426022047e0573ab74730f4776ca53d653b884918225b5a869cb20890067faf14319ee9)

val path = BIP32Path(Vector(BIP32Node(0,false)))
// path: BIP32Path = m/0

extPrivKey.sign(DoubleSha256Digest.empty.bytes,path)
// res1: ECDigitalSignature = ECDigitalSignature(3045022100c799248b456a4ae3512d143d75e03ca0c6f26d845b39353ca9de8b3b2dff7a1e0220563285830a5a43562a35c005ff996911535ad70c38930f163c26b1650e05610c)
```

With `ExtSign`, you can use `ExtPrivateKey` to sign transactions inside of `TxBuilder` since `UTXOSpendingInfo` takes in `Sign` as a parameter. 

You can also provide a `path` to use to derive a child `ExtPrivateKey`, and then sign with that child private key