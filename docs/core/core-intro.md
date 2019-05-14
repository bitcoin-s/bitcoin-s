---
id: core-intro
title: Core module
---

The `core` module is the core (duh!) functionality of Bitcoin-S. The goal is to provide basic
data structures that are found in the Bitcoin and Lightning protocols while
minimizing external depedencies for security purposes. We aim to have an extremely
high level of test coverage in this module to flesh out bugs. We use property based
testing heavily in this library to ensure high quality of code.

## The basics

Every bitcoin protocol data structure (and some other data structures) extends [`NetworkElement`](/api/org/bitcoins/core/protocol/NetworkElement). `NetworkElement` provides methods to convert the data structure to hex or byte representation. When paired with [`Factory`](/api/org/bitcoins/core/util/Factory) we can easily serialize and deserialize data structures.

Most data structures have companion objects that extends `Factory` to be able to easily create protocol data structures. An example of this is the [`ScriptPubKey`](/api/org/bitcoins/core/protocol/script/ScriptPubKey) companion object. You can use this companion object to create a `ScriptPubKey` from a hex string or a byte array.

## Main modules in `core`

1. [`protocol`](/api/org/bitcoins/core/protocol) - basic protocol data structures. Useful for serializing/deserializing things
1. [`crypto`](/api/org/bitcoins/core/crypto) - cryptograhic functionality used in Bitcoin and Lightning
1. [`script`](/api/org/bitcoins/core/script) - an implementation of [Script](https://en.bitcoin.it/wiki/Script) - the programming language in Bitcoin
1. [`wallet`](/api/org/bitcoins/core/wallet) - implements signing logic for Bitcoin transactions. This module is not named well as there is **NO** functionality to persist wallet state to disk as it stands. This will most likely be renamed in the future.
1. [`config`](/api/org/bitcoins/core/config) - Contains information about a chain's genesis block and DNS seeds
1. [`number`](/api/org/bitcoins/core/number) - Implements number types that are native in C, i.e. `UInt8`, `UInt32`, `UInt64`, etc.
1. [`currency`](/api/org/bitcoins/core/currency) - Implements currency units in the Bitcoin protocol
1. [`bloom`](/api/org/bitcoins/core/bloom) - Implements [Bloom filters](https://en.wikipedia.org/wiki/Bloom_filter) and [merkle blocks](https://bitcoin.org/en/glossary/merkle-block) needed for [BIP37](https://github.com/bitcoin/bips/blob/master/bip-0037.mediawiki)
1. [`hd`](/api/org/bitcoins/core/hd) - Contains implementations of hierarchical deterministic (HD) paths, that when combined with `ExtPrivKey` and `ExtPubKey` in `crypto` can implement BIP32, BIP44, BIP49 and BIP84.

## Examples

### Serializing and deserializing a `Transaction`

Here is an example scala console session with bitcoins-core

```scala mdoc
import org.bitcoins.core.protocol.transaction._

val hexTx = "0100000001ccf318f0cbac588a680bbad075aebdda1f211c94ba28125b0f627f9248310db3000000006b4830450221008337ce3ce0c6ac0ab72509f8$9c1d52701817a2362d6357457b63e3bdedc0c0602202908963b9cf1a095ab3b34b95ce2bc0d67fb0f19be1cc5f7b3de0b3a325629bf01210241d746ca08da0a668735c3e01c1$a02045f2f399c5937079b6434b5a31dfe353ffffffff0210335d05000000001976a914b1d7591b69e9def0feb13254bace942923c7922d88ac48030000000000001976a9145e$90c865c2f6f7a9710a474154ab1423abb5b9288ac00000000"

// val tx = Transaction.fromHex(hexTx)

// val hexAgain = tx.hex
```

This gives us an example of a hex encoded Bitcoin transaction that is deserialized to a native Scala object called a [`Transaction`](/api/org/bitcoins/core/protocol/transaction/Transaction). You could also serialize the transaction to bytes using `tx.bytes` instead of `tx.hex`. These methods are available on every data structure that extends NetworkElement, like [`ECPrivateKey`](/api/org/bitcoins/core/crypto/ECPrivateKey), [`ScriptPubKey`](/api/org/bitcoins/core/protocol/script/ScriptPubKey), [`ScriptWitness`](/api/org/bitcoins/core/protocol/script/ScriptWitness), and [`Block`](/api/org/bitcoins/core/protocol/blockchain/Block).

#### Generating a BIP39 mnemonic phrase and an `xpriv`

BIP39 mnemonic phrases are the most common way of creating backups of wallets.
They are between 12 and 24 words the user writes down, and can later be used to restore
their bitcoins. From the mnemonic phrase we generate a wallet seed, and that seed
can be used to generate what's called an extended private key
([`ExtPrivateKey`](/api/org/bitcoins/core/crypto/ExtPrivateKey) in Bitcoin-S).

Here's an example:

```scala mdoc:to-string
import scodec.bits._
import org.bitcoins.core.crypto._
import org.bitcoins.core.hd._

// the length of the entropy bit vector determine
// how long our phrase ends up being
// 256 bits of entropy results in 24 words
val entropy: BitVector = MnemonicCode.getEntropy256Bits

val mnemonicCode = MnemonicCode.fromEntropy(entropy)

mnemonicCode.words // the phrase the user should write down

// the password argument is an optional, extra security
// measure. all MnemonicCode instances will give you a
// valid BIP39 seed, but different passwords will give
// you different seeds. So you could have as many wallets
// from the same seed as you'd like, by simply giving them
// different passwords.
val bip39Seed = BIP39Seed.fromMnemonic(mnemonicCode,
                                       password = "secret password")

val xpriv = ExtPrivateKey.fromBIP39Seed(ExtKeyVersion.SegWitMainNetPriv,
                                        bip39Seed)
val xpub = xpriv.extPublicKey

// you can now use the generated xpriv to derive further
// private or public keys

// this can be done with BIP89 paths (called SegWitHDPath in bitcoin-s)
val segwitPath = SegWitHDPath.fromString("m/84'/0'/0'/0/0")

// alternatively:
val otherSegwitPath =
SegWitHDPath(HDCoinType.Bitcoin,
             accountIndex = 0,
             HDChainType.External,
             addressIndex = 0)

segwitPath == otherSegwitPath

// there's also paths available for legacy
// addresses (LegacyHDPath) as well as nested
// segwit paths (NestedSegWitPath)
```

### Building a signed transaction

Bitcoin Core supports building unsigned transactions and then signing them with a set of private keys. The first important thing to look at is [`UTXOSpendingInfo`](/api/org/bitcoins/core/wallet/utxo/UTXOSpendingInfo). This contains all of the information needed to create a validly signed [`ScriptSignature`](/api/org/bitcoins/core/protocol/script/ScriptSignature) that spends this output.

Our [`TxBuilder`](/api/org/bitcoins/core/wallet/builder/TxBuilder) class requires you to provide the following:

1. `destinations` - the places we are sending bitcoin to. These are [TransactionOutputs](/api/org/bitcoins/core/protocol/transaction/TransactionOutput) you are sending coins too
2. `utxos` - these are the [UTXOs](/api/org/bitcoins/core/wallet/utxo/UTXOSpendingInfo) used to fund your transaction. These must exist in your wallet, and you must know how to spend them (i.e. have the private key)
3. `feeRate` - the fee rate you want to pay for this transaction
4. `changeSPK` - where the change (i.e. `creditingAmount - destinationAmount - fee`) from the transaction will be sent
5. `network` - the network(/api/org/bitcoins/core/config/NetworkParameters) we are transacting on

After providing this information, you can generate a validly signed bitcoin transaction by calling the `sign` method.

To see a complete example of this, see [our `TxBuilder` example](txbuilder.md)

### The [`Sign` API](/api/org/bitcoins/core/crypto/Sign)

This is the API we define to sign things with. It takes in an arbitrary byte vector and returns a `Future[ECDigitalSignature]`. The reason we incorporate `Future`s here is for extensibility of this API. We would like to provide implementations of this API for hardware devices, which need to be asynchrnous since they may require user input.

From [`core/src/main/scala/org/bitcoins/core/crypto/Sign.scala`](/api/org/bitcoins/core/crypto/Sign):

```scala mdoc
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

### Verifying a transaction's script is valid (does not check if UTXO is valid)

Transactions are run through the interpreter to check their validity. These are packaged up into an object called `ScriptProgram`, which contains the following:

- The transaction that is being checked
- The specific input index that it is checking
- The `scriptPubKey` for the crediting transaction
- The flags used to verify the script

Here is an example of a transaction spending a `scriptPubKey` which is correctly evaluated with our interpreter implementation:

```scala mdoc:silent
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script._
import org.bitcoins.core.script.interpreter._
import org.bitcoins.core.policy._
import org.bitcoins.core.number._
import org.bitcoins.core.crypto._
import org.bitcoins.core.currency._

val spendingTx = Transaction.fromHex("0100000001ccf318f0cbac588a680bbad075aebdda1f211c94ba28125b0f627f9248310db3000000006b4830450221008337ce3ce0c6ac0ab72509f889c1d52701817a2362d6357457b63e3bdedc0c0602202908963b9cf1a095ab3b34b95ce2bc0d67fb0f19be1cc5f7b3de0b3a325629bf01210241d746ca08da0a668735c3e01c1fa02045f2f399c5937079b6434b5a31dfe353ffffffff0210335d05000000001976a914b1d7591b69e9def0feb13254bace942923c7922d88ac48030000000000001976a9145e690c865c2f6f7a9710a474154ab1423abb5b9288ac00000000")

val scriptPubKey = ScriptPubKey.fromAsmHex("76a91431a420903c05a0a7de2de40c9f02ebedbacdc17288ac")

val output = TransactionOutput(CurrencyUnits.zero, scriptPubKey)

val inputIndex = UInt32.zero

val btxsc = BaseTxSigComponent(spendingTx,inputIndex,output,Policy.standardScriptVerifyFlags)

val preExecution = PreExecutionScriptProgram(btxsc)
```

```scala mdoc
val result = ScriptInterpreter.run(preExecution)
```
