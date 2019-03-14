[ ![Download](https://api.bintray.com/packages/bitcoin-s/bitcoin-s-core/bitcoin-s-core/images/download.svg) ](https://bintray.com/bitcoin-s/bitcoin-s-core/bitcoin-s-core/_latestVersion)

# `core` module

This is the core functionality of Bitcoin-S. The goal is to provide basic data structures that are found in the Bitcoin and Lightning protocols while minimizing external depedencies for security purposes. We aim to have an extremely high level of test coverage in this module to flesh out bugs. We use [property based testing](http://www.scalatest.org/user_guide/property_based_testing) heavily in this library to ensure high quality of code.

## The basics

Every bitcoin protocol data structure (and some other data structures) extends [`NetworkElement`](src/main/scala/org/bitcoins/core/protocol/NetworkElement.scala). `NetworkElement` provides methods to convert the data structure to hex or byte representation. When paired with [`Factory`](src/main/scala/org/bitcoins/core/util/Factory.scala) we can easily serialize and deserialize data structures.

Most data structures have companion objects that extends `Factory` to be able to easily create protocol data structures. An example of this is the [`ScriptPubKey`](https://github.com/bitcoin-s/bitcoin-s-core/blob/1c7a7b9f46679a753248d9f55246c272bb3d63b9/src/main/scala/org/bitcoins/core/protocol/script/ScriptPubKey.scala#L462) companion object. You can use this companion object to create a `ScriptPubKey` from a hex string or a byte array.

## Main modules in `core`

1. [`protocol`](src/main/scala/org/bitcoins/core/protocol) - basic protocol data structures. Useful for serializing/deserializing things
2. [`script`](src/main/scala/org/bitcoins/core/script) - an implementation of [Script](https://en.bitcoin.it/wiki/Script) - the programming language in Bitcoin
3. [`wallet`](src/main/scala/org/bitcoins/core/wallet) - implements signing logic for Bitcoin transactions. This module is not named well as there is **NO** functionality to persist wallet state to disk as it stands. This will most likely be renamed in the future.
4. [`config`](src/main/scala/org/bitcoins/core/config) - Contains information about a chain's genesis block and DNS seeds
5. [`number`](src/main/scala/org/bitcoins/core/number) - Implements number types that are native in C, i.e. `UInt8`, `UInt32`, `UInt64`, etc.
6. [`currency`](src/main/scala/org/bitcoins/core/currency) - Implements currency units in the Bitcoin protocol
7. [`bloom`](src/main/scala/org/bitcoins/core/bloom) - Implements [Bloom filters](https://en.wikipedia.org/wiki/Bloom_filter) and [merkle blocks](https://bitcoin.org/en/glossary/merkle-block) needed for [BIP37](https://github.com/bitcoin/bips/blob/master/bip-0037.mediawiki)

## Examples

### Serializing and deserializing a `Transaction`

Here is an example scala console session with bitcoins-core

```scala
$ sbt core/console
[info] Loading global plugins from /home/chris/.sbt/0.13/plugins
[info] Loading project definition from /home/chris/dev/bitcoin-s-core/project
[info] Set current project to bitcoin-s-core (in build file:/home/chris/dev/bitcoin-s-core/)
[info] Starting scala interpreter...
[info]
Welcome to Scala version 2.11.7 (Java HotSpot(TM) 64-Bit Server VM, Java 1.8.0_151).
Type in expressions to have them evaluated.
Type :help for more information.

scala> import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.protocol.transaction._

scala> val hexTx = "0100000001ccf318f0cbac588a680bbad075aebdda1f211c94ba28125b0f627f9248310db3000000006b4830450221008337ce3ce0c6ac0ab72509f8$9c1d52701817a2362d6357457b63e3bdedc0c0602202908963b9cf1a095ab3b34b95ce2bc0d67fb0f19be1cc5f7b3de0b3a325629bf01210241d746ca08da0a668735c3e01c1$a02045f2f399c5937079b6434b5a31dfe353ffffffff0210335d05000000001976a914b1d7591b69e9def0feb13254bace942923c7922d88ac48030000000000001976a9145e$90c865c2f6f7a9710a474154ab1423abb5b9288ac00000000"
hexTx: String = 0100000001ccf318f0cbac588a680bbad075aebdda1f211c94ba28125b0f627f9248310db3000000006b4830450221008337ce3ce0c6ac0ab72509f889c1$52701817a2362d6357457b63e3bdedc0c0602202908963b9cf1a095ab3b34b95ce2bc0d67fb0f19be1cc5f7b3de0b3a325629bf01210241d746ca08da0a668735c3e01c1fa02$45f2f399c5937079b6434b5a31dfe353ffffffff0210335d05000000001976a914b1d7591b69e9def0feb13254bace942923c7922d88ac48030000000000001976a9145e690c$65c2f6f7a9710a474154ab1423abb5b9288ac00000000

scala> val tx = Transaction.fromHex(hexTx)
tx: org.bitcoins.core.protocol.transaction.Transaction = BaseTransactionImpl(UInt32Impl(1),List(TransactionInputImpl(TransactionOutPointImpl$DoubleSha256DigestImpl(ccf318f0cbac588a680bbad075aebdda1f211c94ba28125b0f627f9248310db3),UInt32Impl(0)),P2PKHScriptSignatureImpl(6b483045022$008337ce3ce0c6ac0ab72509f889c1d52701817a2362d6357457b63e3bdedc0c0602202908963b9cf1a095ab3b34b95ce2bc0d67fb0f19be1cc5f7b3de0b3a325629bf012102$1d746ca08da0a668735c3e01c1fa02045f2f399c5937079b6434b5a31dfe353),UInt32Impl(4294967295))),List(TransactionOutputImpl(SatoshisImpl(Int64Impl($9994000)),P2PKHScriptPubKeyImpl(1976a914b1d7591b69e9def0feb13254bace942923c7922d88ac)), TransactionOutputImpl(SatoshisImpl(Int64Impl(840)),P$PKHScriptPubKeyImpl(1976a9145e690c865c2f6f7a9710a474154ab1423abb5b9288ac))),UInt32Impl(0))

scala> val hexAgain = tx.hex
hexAgain: String = 0100000001ccf318f0cbac588a680bbad075aebdda1f211c94ba28125b0f627f9248310db3000000006b4830450221008337ce3ce0c6ac0ab72509f88$c1d52701817a2362d6357457b63e3bdedc0c0602202908963b9cf1a095ab3b34b95ce2bc0d67fb0f19be1cc5f7b3de0b3a325629bf01210241d746ca08da0a668735c3e01c1f$02045f2f399c5937079b6434b5a31dfe353ffffffff0210335d05000000001976a914b1d7591b69e9def0feb13254bace942923c7922d88ac48030000000000001976a9145e6$0c865c2f6f7a9710a474154ab1423abb5b9288ac00000000

```

This gives us an example of a hex encoded Bitcoin transaction that is deserialized to a native Scala object called a [`Transaction`](https://github.com/bitcoin-s/bitcoin-s-core/blob/6358eb83067909771f989d615b422759222d060a/src/main/scala/org/bitcoins/core/protocol/transaction/Transaction.scala#L14-L42). You could also serialize the transaction to bytes using `tx.bytes` instead of `tx.hex`. These methods are available on every data structure that extends NetworkElement, like [`ECPrivateKey`](https://github.com/bitcoin-s/bitcoin-s-core/blob/6358eb83067909771f989d615b422759222d060a/src/main/scala/org/bitcoins/core/crypto/ECKey.scala#L23-L67), [`ScriptPubKey`](https://github.com/bitcoin-s/bitcoin-s-core/blob/6358eb83067909771f989d615b422759222d060a/src/main/scala/org/bitcoins/core/protocol/script/ScriptPubKey.scala#L23), [`ScriptWitness`](https://github.com/bitcoin-s/bitcoin-s-core/blob/6358eb83067909771f989d615b422759222d060a/src/main/scala/org/bitcoins/core/protocol/script/ScriptWitness.scala#L13), and [`Block`](https://github.com/bitcoin-s/bitcoin-s-core/blob/6358eb83067909771f989d615b422759222d060a/src/main/scala/org/bitcoins/core/protocol/blockchain/Block.scala#L17).

#### Generating a BIP39 mnemonic phrase and an `xpriv`

BIP39 mnemonic phrases are the most common way of creating backups of wallets.
They are between 12 and 24 words the user writes down, and can later be used to restore 
their bitcoins. From the mnemonic phrase we generate a wallet seed, and that seed 
can be used to generate what's called an extended private key 
([`ExtPrivateKey`](src/main/scala/org/bitcoins/core/crypto/ExtKey.scala) in Bitcoin-S).

Here's an example: 

```scala
import scodec.bits._
import org.bitcoins.core.crypto._

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
                                   
val xpriv = ExtPrivateKey.fromBIP39Seed(ExtKeyVersion.MainNetPriv,  // or testnet/regtest
                                        bip39Seed)
                                      
// you can now use the generated xpriv to derive further
// private or public keys

```


### Building a signed transaction

Bitcoin Core supports building unsigned transactions and then signing them with a set of private keys. The first important thing to look at is [`UTXOSpendingInfo`](src/main/scala/org/bitcoins/core/wallet/utxo/UTXOSpendingInfo.scala). This contains all of the information needed to create a validly signed [`ScriptSignature`](src/main/scala/org/bitcoins/core/protocol/script/ScriptSignature.scala) that spends this output.

Our [`TxBuilder`](src/main/scala/org/bitcoins/core/wallet/builder/TxBuilder.scala) class requires you to provide the following:

1. `destinations` - the places we are sending bitcoin to. These are [TransactionOutputs](src/main/scala/org/bitcoins/core/protocol/transaction/TransactionOutput.scala) you are sending coins too
2. `utxos` - these are the [UTXOs](src/main/scala/org/bitcoins/core/wallet/utxo/UTXOSpendingInfo.scala) used to fund your transaction. These must exist in your wallet, and you must know how to spend them (i.e. have the private key)
3. `feeRate` - the fee rate you want to pay for this transaction
4. `changeSPK` - where the change (i.e. `creditingAmount - destinationAmount - fee`) from the transaction will be sent
5. `network` - the [`Network`](src/main/scala/org/bitcoins/core/config/NetworkParameters.scala) we are transacting on

After providing this information, you can generate a validly signed bitcoin transaction by calling the `sign` method.

### The [`Sign` API](src/main/scala/org/bitcoins/core/crypto/Sign.scala)

This is the API we define to sign things with. It takes in an arbitrary byte vector and returns a `Future[ECDigitalSignature]`. The reason we incorporate `Future`s here is for extensibility of this API. We would like to provide implementations of this API for hardware devices, which need to be asynchrnous since they may require user input.

From [`core/src/main/scala/org/bitcoins/core/crypto/Sign.scala`](src/main/scala/org/bitcoins/core/crypto/Sign.scala):

```scala
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

The `ByteVector` that is input to the `signFunction` should be the hash that is output from [`TransactionSignatureSerializer`](src/main/scala/org/bitcoins/core/crypto/TransactionSignatureSerializer.scala)'s `hashForSignature` method. Our in-memory [`ECKey`](src/main/scala/org/bitcoins/core/crypto/ECKey.scala) types implement the `Sign` API.

If you wanted to implement a new `Sign` api for a hardware wallet, you can easily pass it into the `TxBuilder`/`Signer` classes to allow for you to use those devices to sign with Bitcoin-S.

This API is currently used to sign ordinary transactions with our [`Signer`](src/main/scala/org/bitcoins/core/wallet/signer/Signer.scala)s. The `Signer` subtypes (i.e. `P2PKHSigner`) implement the specific functionality needed to produce a valid digital signature for their corresponding script type.

#### Complete `TxBuilder` example

For an example of how to use the `TxBuilder` please see [`TxBuilderExample.scala`](../doc/src/test/scala/TxBuilderExample.scala).

### Verifying a transaction's script is valid (does not check if UTXO is valid)

Transactions are run through the interpreter to check their validity. These are packaged up into an object called `ScriptProgram`, which contains the following:

- The transaction that is being checked
- The specific input index that it is checking
- The `scriptPubKey` for the crediting transaction
- The flags used to verify the script

Here is an example of a transaction spending a `scriptPubKey` which is correctly evaluated with our interpreter implementation:

```scala
chris@chris:~/dev/bitcoins-core$ sbt core/console

scala> import org.bitcoins.core.protocol.script._
scala> import org.bitcoins.core.protocol.transaction._
scala> import org.bitcoins.core.script._
scala> import org.bitcoins.core.script.interpreter._
scala> import org.bitcoins.core.policy._
scala> import org.bitcoins.core.number._
scala> import org.bitcoins.core.crypto._
scala> import org.bitcoins.core.currency._

scala> val spendingTx = Transaction.fromHex("0100000001ccf318f0cbac588a680bbad075aebdda1f211c94ba28125b0f627f9248310db3000000006b4830450221008337ce3ce0c6ac0ab72509f889c1d52701817a2362d6357457b63e3bdedc0c0602202908963b9cf1a095ab3b34b95ce2bc0d67fb0f19be1cc5f7b3de0b3a325629bf01210241d746ca08da0a668735c3e01c1fa02045f2f399c5937079b6434b5a31dfe353ffffffff0210335d05000000001976a914b1d7591b69e9def0feb13254bace942923c7922d88ac48030000000000001976a9145e690c865c2f6f7a9710a474154ab1423abb5b9288ac00000000")
spendingTx: org.bitcoins.core.protocol.transaction.Transaction = ... // omitted for brevity

scala> val scriptPubKey = ScriptPubKey.fromAsmHex("76a91431a420903c05a0a7de2de40c9f02ebedbacdc17288ac")
scriptPubKey: org.bitcoins.core.protocol.script.ScriptPubKey = P2PKHScriptPubKeyImpl(1976a91431a420903c05a0a7de2de40c9f02ebedbacdc17288ac)

scala> val output = TransactionOutput(CurrencyUnits.zero, scriptPubKey)
output: org.bitcoins.core.protocol.transaction.TransactionOutput = ... // omitted for brevity

scala> val inputIndex = UInt32.zero
inputIndex: org.bitcoins.core.number.UInt32 = UInt32Impl(0)

scala> val btxsc = BaseTxSigComponent(spendingTx,inputIndex,output,Policy.standardScriptVerifyFlags)
btxsc: org.bitcoins.core.crypto.BaseTxSigComponent = ... // omitted for brevity

scala> val preExecution = PreExecutionScriptProgram(btxsc)
preExecution: org.bitcoins.core.script.PreExecutionScriptProgram = ... // omitted for brevity

scala> val result = ScriptInterpreter.run(preExecution)
result: org.bitcoins.core.script.result.ScriptResult = ScriptOk
```

## Running `core` tests

See [`core-test/README.md`](../core-test/README.md).

