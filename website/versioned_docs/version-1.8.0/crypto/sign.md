---
id: version-1.8.0-sign
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
// res0: ECDigitalSignature = ECDigitalSignature(304402200d093e8d339ebd7e340d4509b4fd7e53da75928644d40e681f0ec9e1b3fe5a3b0220208f9c8e6e403a95c26cca66823f2739cfbe1d9b5ab15ce39d23fd8635f0fd1b)

val path = BIP32Path(Vector(BIP32Node(0,false)))
// path: BIP32Path = m/0

extPrivKey.sign(DoubleSha256Digest.empty.bytes,path)
// res1: ECDigitalSignature = ECDigitalSignature(3044022050641ec9e6ac14017cb8c812e83e48ee6bb1862215f06ac9a88399be490dcdcc02203202049ce78e8965964d315024d97f6df41a066930cf024379fd5bfc7cc5c0ed)
```

With `ExtSign`, you can use `ExtPrivateKey` to sign transactions inside of `TxBuilder` since `UTXOSpendingInfo` takes in `Sign` as a parameter. 

You can also provide a `path` to use to derive a child `ExtPrivateKey`, and then sign with that child private key