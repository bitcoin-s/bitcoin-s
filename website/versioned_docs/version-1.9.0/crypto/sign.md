---
id: version-1.9.0-sign
title: Sign API
original_id: sign
---

### The [`Sign` API](/api/org/bitcoins/crypto/Sign)

This is the API we define to sign things with. It takes in an arbitrary byte vector and returns a `Future[ECDigitalSignature]`. The reason we incorporate `Future`s here is for extensibility of this API. We would like to provide implementations of this API for hardware devices, which need to be asynchrnous since they may require user input.

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
// res0: ECDigitalSignature = ECDigitalSignature(3045022100bc9a8cdcd0225de335b12e4bcf0a202950b3616203d96226e97ca31834c2fa0c0220516a7c626137fdb1646dcae952c5abfd131c832b9dafa694d3e539bae212a478)

val path = BIP32Path(Vector(BIP32Node(0,false)))
// path: BIP32Path = m/0

extPrivKey.sign(DoubleSha256Digest.empty.bytes,path)
// res1: ECDigitalSignature = ECDigitalSignature(304402201b8d99718778708902f1677a151164fda4a2420e5b5424b5fa5bec66c3aa707b02206e332a03c30eb886e29e64dda669daa9e3c16749cd11ec41c84f46276d9b2e0c)
```

With `ExtSign`, you can use `ExtPrivateKey` to sign transactions inside of `TxBuilder` since `UTXOSpendingInfo` takes in `Sign` as a parameter. 

You can also provide a `path` to use to derive a child `ExtPrivateKey`, and then sign with that child private key