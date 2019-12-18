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
[info] Starting scala interpreter...
Welcome to Scala 2.13.1 (OpenJDK 64-Bit GraalVM CE 19.3.0, Java 1.8.0_232).
Type in expressions for evaluation. Or try :help.

scala> import org.bitcoins.core.hd._
import org.bitcoins.core.hd._

scala> import org.bitcoins.core.crypto._
import org.bitcoins.core.crypto._

scala> val extPrivKey = ExtPrivateKey(ExtKeyVersion.SegWitMainNetPriv)
SLF4J: Failed to load class "org.slf4j.impl.StaticLoggerBinder".
SLF4J: Defaulting to no-operation (NOP) logger implementation
SLF4J: See http://www.slf4j.org/codes.html#StaticLoggerBinder for further details.
extPrivKey: org.bitcoins.core.crypto.ExtPrivateKey = zprvAWgYBBk7JR8GkbLHbSiLjaZ5MrxyErnQaeAMiiTK8BzfFE3r1ECF35GCKr3TSakAfshSGoqKGCz6Lcm567dExL3Hj9nyXm5GpQeq4WrLLhs

scala> extPrivKey.sign(DoubleSha256Digest.empty.bytes)
res0: org.bitcoins.core.crypto.ECDigitalSignature = ECDigitalSignature(304402203591cf49b08dae0c9830b0d0f955b329ec8bf033f91fb63b4e33bbaa92d55e3502206e7c498c0362ae1dadf3071eb45c23c77b347b062b1794d036bd63c3d73c13d7)

scala> val path = BIP32Path(Vector(BIP32Node(0,false)))
path: org.bitcoins.core.hd.BIP32Path = m/0

scala> extPrivKey.sign(DoubleSha256Digest.empty.bytes,path)
res4: org.bitcoins.core.crypto.ECDigitalSignature = ECDigitalSignature(304402203784368d19555227bdb9d96360d628f3fdea1e5ca189d125d534c1f0c891f5fb02203c5efd41366045d2f6ce69ccaed706924270cd4fa8bfbf76d29f7bbfecf27eb3)
```

With `ExtSign`, you can use `ExtPrivateKey` to sign transactions inside of `TxBuilder` since `UTXOSpendingInfo` takes in `Sign` as a parameter. 

You can also provide a `path` to use to derive a child `ExtPrivateKey`, and then sign with that child private key