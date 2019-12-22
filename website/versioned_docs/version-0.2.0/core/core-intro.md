---
id: version-0.2.0-core-intro
title: Core module
original_id: core-intro
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

```scala
import org.bitcoins.core.protocol.transaction._

val hexTx = "0100000002d8c8df6a6fdd2addaf589a83d860f18b44872d13ee6ec3526b2b470d42a96d4d000000008b483045022100b31557e47191936cb14e013fb421b1860b5e4fd5d2bc5ec1938f4ffb1651dc8902202661c2920771fd29dd91cd4100cefb971269836da4914d970d333861819265ba014104c54f8ea9507f31a05ae325616e3024bd9878cb0a5dff780444002d731577be4e2e69c663ff2da922902a4454841aa1754c1b6292ad7d317150308d8cce0ad7abffffffff2ab3fa4f68a512266134085d3260b94d3b6cfd351450cff021c045a69ba120b2000000008b4830450220230110bc99ef311f1f8bda9d0d968bfe5dfa4af171adbef9ef71678d658823bf022100f956d4fcfa0995a578d84e7e913f9bb1cf5b5be1440bcede07bce9cd5b38115d014104c6ec27cffce0823c3fecb162dbd576c88dd7cda0b7b32b0961188a392b488c94ca174d833ee6a9b71c0996620ae71e799fc7c77901db147fa7d97732e49c8226ffffffff02c0175302000000001976a914a3d89c53bb956f08917b44d113c6b2bcbe0c29b788acc01c3d09000000001976a91408338e1d5e26db3fce21b011795b1c3c8a5a5d0788ac00000000"
// hexTx: String = 0100000002d8c8df6a6fdd2addaf589a83d860f18b44872d13ee6ec3526b2b470d42a96d4d000000008b483045022100b31557e47191936cb14e013fb421b1860b5e4fd5d2bc5ec1938f4ffb1651dc8902202661c2920771fd29dd91cd4100cefb971269836da4914d970d333861819265ba014104c54f8ea9507f31a05ae325616e3024bd9878cb0a5dff780444002d731577be4e2e69c663ff2da922902a4454841aa1754c1b6292ad7d317150308d8cce0ad7abffffffff2ab3fa4f68a512266134085d3260b94d3b6cfd351450cff021c045a69ba120b2000000008b4830450220230110bc99ef311f1f8bda9d0d968bfe5dfa4af171adbef9ef71678d658823bf022100f956d4fcfa0995a578d84e7e913f9bb1cf5b5be1440bcede07bce9cd5b38115d014104c6ec27cffce0823c3fecb162dbd576c88dd7cda0b7b32b0961188a392b488c94ca174d833ee6a9b71c0996620ae71e799fc7c77901db147fa7d97732e49c8226ffffffff02c0175302000000001976a914a3d89c53bb956f08917b44d113c6b2bcbe0c29b788acc01c3d09000000001976a91408338e1d5e26db3fce21b011795b1c3c8a5a5d0788ac00000000

val tx = Transaction.fromHex(hexTx)
// tx: Transaction = BaseTransactionImpl(Int32Impl(1),Vector(TransactionInputImpl(TransactionOutPoint(4d6da9420d472b6b52c36eee132d87448bf160d8839a58afdd2add6f6adfc8d8:0),P2PKHScriptSignature(ECPublicKey(04c54f8ea9507f31a05ae325616e3024bd9878cb0a5dff780444002d731577be4e2e69c663ff2da922902a4454841aa1754c1b6292ad7d317150308d8cce0ad7ab), ECDigitalSignature(3045022100b31557e47191936cb14e013fb421b1860b5e4fd5d2bc5ec1938f4ffb1651dc8902202661c2920771fd29dd91cd4100cefb971269836da4914d970d333861819265ba01)),UInt32Impl(4294967295)), TransactionInputImpl(TransactionOutPoint(b220a19ba645c021f0cf501435fd6c3b4db960325d0834612612a5684ffab32a:0),P2PKHScriptSignature(ECPublicKey(04c6ec27cffce0823c3fecb162dbd576c88dd7cda0b7b32b0961188a392b488c94ca174d833ee6a9b71c0996620ae71e799fc7c77901db147fa7d97732e49c8226), ECDigitalSignature(30450220230110bc99ef311f1f8bda9d0d968bfe5dfa4af171adbef9ef71678d658823bf022100f956d4fcfa0995a578d84e7e913f9bb1cf5b5be1440bcede07bce9cd5b38115d01)),UInt32Impl(4294967295))),Vector(TransactionOutput(39000000 sats,P2PKHScriptPubKeyImpl(Sha256Hash160DigestImpl(a3d89c53bb956f08917b44d113c6b2bcbe0c29b7))), TransactionOutput(155000000 sats,P2PKHScriptPubKeyImpl(Sha256Hash160DigestImpl(08338e1d5e26db3fce21b011795b1c3c8a5a5d07)))),UInt32Impl(0))

tx.hex == hexTx
// res0: Boolean = true
```

This gives us an example of a hex encoded Bitcoin transaction that is deserialized to a native Scala object called a [`Transaction`](/api/org/bitcoins/core/protocol/transaction/Transaction). You could also serialize the transaction to bytes using `tx.bytes` instead of `tx.hex`. These methods are available on every data structure that extends NetworkElement, like [`ECPrivateKey`](/api/org/bitcoins/core/crypto/ECPrivateKey), [`ScriptPubKey`](/api/org/bitcoins/core/protocol/script/ScriptPubKey), [`ScriptWitness`](/api/org/bitcoins/core/protocol/script/ScriptWitness), and [`Block`](/api/org/bitcoins/core/protocol/blockchain/Block).

#### Generating a BIP39 mnemonic phrase and an `xpriv`

See our [HD example](hd-keys)

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

### Verifying a transaction's script is valid (does not check if UTXO is valid)

Transactions are run through the interpreter to check their validity. These are packaged up into an object called `ScriptProgram`, which contains the following:

- The transaction that is being checked
- The specific input index that it is checking
- The `scriptPubKey` for the crediting transaction
- The flags used to verify the script

Here is an example of a transaction spending a `scriptPubKey` which is correctly evaluated with our interpreter implementation:

```scala
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

```scala
val result = ScriptInterpreter.run(preExecution)
// result: org.bitcoins.core.script.result.ScriptResult = ScriptOk
```
