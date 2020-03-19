---
id: version-0.3.0-key-manager
title: Key Manager
original_id: key-manager
---


### Key Manager

The key manager module's goal is to encapusulate all private key interactions with the [wallet](../wallet/wallet.md) project.

As of this writing, there is only one type of `KeyManager` - [`BIP39KeyManager`](../../key-manager/src/main/scala/org/bitcoins/keymanager/bip39/BIP39KeyManager.scala). 

The [`BIP39KeyManager`](../../key-manager/src/main/scala/org/bitcoins/keymanager/bip39/BIP39KeyManager.scala) stores a [`MnemonicCode`](../../core/src/main/scala/org/bitcoins/core/crypto/MnemonicCode.scala) on disk which can be decrypted and used as a hot wallet.
 
Over the long run, we want to make it so that the wallet project needs to communicate with the key-manager to access private keys.

This means that ALL SIGNING should be done inside of the key-manager, and private keys should not leave the key manager.

This makes it easier to reason about the security characteristics of our private keys, and a way to provide a uniform interface for alternative key storage systems (hsm, cloud based key storage, etc) to be plugged into the bitcoin-s library.

### Disclaimer 

Currently bip39 password is supported at the library level, but is not supported for end users using the server project. 
[You can see that the bip39 password is hard coded to `None` here](https://github.com/bitcoin-s/bitcoin-s/blob/e387d075b0ff2e0a0fec15788fcb48e4ddc4d9d5/app/server/src/main/scala/org/bitcoins/server/Main.scala#L53).

There is a password that is used to encrypt your mnemonic seed on disk, but that password is hard coded to a default value. 
THIS MEANS THAT YOUR MNEMONIC SEED CAN TRIVIALLY BE STOLEN IF AN ATTACKER CAN ACCESS YOUR HARD DRIVE. 
TAKE PROPER OPSEC PRECAUTIONS.

Overall the key manager module should be considered insecure. For this release, it is more about setting up the module 
as a logical distinction for further development in subsequent releases.

#### Creating a key manager

The first thing you need create a key manager is some entropy.

A popular way for bitcoin wallet's to represent entropy is [BIP39](https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki) which you [can use in bitcoin-s](../../core/src/main/scala/org/bitcoins/core/crypto/BIP39Seed.scala)

You can generate a `MnemonicCode` in bitcoin-s with the following code

```scala
import org.bitcoins.core.crypto._

//get 256 bits of random entropy
val entropy = MnemonicCode.getEntropy256Bits
// entropy: scodec.bits.BitVector = BitVector(256 bits, 0xbf2968260f174937863a69ac8e2e8f25813b5437246077b6c3f88f95ef932d76)

val mnemonic = MnemonicCode.fromEntropy(entropy)
// mnemonic: MnemonicCode = Masked(MnemonicCodeImpl)

//you can print that mnemonic seed with this
println(mnemonic.words)
// Vector(sand, ensure, another, bullet, innocent, orange, body, essay, prosper, imitate, phrase, enrich, beauty, present, symptom, metal, jeans, render, wreck, busy, galaxy, sister, remind, sort)
```

Now that we have a `MnemonicCode` that was securely generated, we need to now create `KeyManagerParams` which tells us how to generate
generate specific kinds of addresses for wallets.

`KeyManagerParams` takes 3 parameters:

1. `seedPath` there is where we store the `MnemonicCode` on your file system
2. [`purpose`](../../core/src/main/scala/org/bitcoins/core/hd/HDPurpose.scala) which represents what type of utxo this `KeyManager` is associated with. The specification for this is in [BIP43](https://github.com/bitcoin/bips/blob/master/bip-0043.mediawiki)
3. [`network`](../../core/src/main/scala/org/bitcoins/core/config/NetworkParameters.scala) what cryptocurrency network this key manager is associated with


This controls how the root key is defined. The combination of `purpose` and `network` determine how the root `ExtKey` is serialized. For more information on how this works please see [hd-keys](../core/hd-keys.md)

Now we can construct a native segwit key manager for the regtest network!

```scala
//this will create a temp directory with the prefix 'key-manager-example` that will
//have a file in it called "encrypted-bitcoin-s-seed.json"
val seedPath = Files.createTempDirectory("key-manager-example").resolve(WalletStorage.ENCRYPTED_SEED_FILE_NAME)
// seedPath: Path = /tmp/key-manager-example6695794117403645345/encrypted-bitcoin-s-seed.json

//let's create a native segwit key manager
val purpose = HDPurposes.SegWit
// purpose: HDPurpose = m/84'

//let's choose regtest as our network
val network = RegTest
// network: RegTest.type = RegTest

val kmParams = KeyManagerParams(seedPath, purpose, network)
// kmParams: KeyManagerParams = KeyManagerParams(/tmp/key-manager-example6695794117403645345/encrypted-bitcoin-s-seed.json,m/84',RegTest)

val km = BIP39KeyManager.initializeWithMnemonic(mnemonic, None, kmParams)
// km: Either[KeyManagerInitializeError, BIP39KeyManager] = Right(BIP39KeyManager(Masked(MnemonicCodeImpl),KeyManagerParams(/tmp/key-manager-example6695794117403645345/encrypted-bitcoin-s-seed.json,m/84',RegTest),None))

val rootXPub = km.right.get.getRootXPub
// rootXPub: ExtPublicKey = vpub5SLqN2bLY4WeZwnivVWfghDdWLN1osw3cJagfGXncZhnZ4T1r9m2D6Be4h6ceRY6YQR2hDLcvExdAq36cuKTKbEDDBn8CAUAjTxn5Y1o9JH

println(rootXPub)
// vpub5SLqN2bLY4WeZwnivVWfghDdWLN1osw3cJagfGXncZhnZ4T1r9m2D6Be4h6ceRY6YQR2hDLcvExdAq36cuKTKbEDDBn8CAUAjTxn5Y1o9JH
```

Which should print something that looks like this

`vpub5SLqN2bLY4WeXxMqwJHJFBEwxSscGB2uDUnsTS3edVjZEwTrQDFDNqoR2xLqARQPabGaXsHSTenTRcqm2EnB9MpuC4vSk3LqSgNmGGZtuq7`

which is a native segwit `ExtPubKey` for the regtest network!

You can always change the `network` or `purpose` to support different things. You do _not_ need to initialize the key manager
again after initializing it once. You can use the same `mnemonic` for different networks, which you control `KeyManagerParams`.

```scala
//let's create a nested segwit key manager for mainnet
val mainnetKmParams = KeyManagerParams(seedPath, HDPurposes.SegWit, MainNet)
// mainnetKmParams: KeyManagerParams = KeyManagerParams(/tmp/key-manager-example6695794117403645345/encrypted-bitcoin-s-seed.json,m/84',MainNet)

//we do not need to all `initializeWithMnemonic()` again as we have saved the seed to dis
val mainnetKeyManager = BIP39KeyManager(mnemonic, mainnetKmParams, None)
// mainnetKeyManager: BIP39KeyManager = BIP39KeyManager(Masked(MnemonicCodeImpl),KeyManagerParams(/tmp/key-manager-example6695794117403645345/encrypted-bitcoin-s-seed.json,m/84',MainNet),None)

val mainnetXpub = mainnetKeyManager.getRootXPub
// mainnetXpub: ExtPublicKey = zpub6jftahH18ngZy8ZCFvfAX3beCCwoaMu3GkfZnr7L8bDJmThvrnRGhLpC9Wvxe49nAxtFh7irktNphyVMVgyWWXxcgYZpXok7pNDMdoTT1K6

println(mainnetXpub)
// zpub6jftahH18ngZy8ZCFvfAX3beCCwoaMu3GkfZnr7L8bDJmThvrnRGhLpC9Wvxe49nAxtFh7irktNphyVMVgyWWXxcgYZpXok7pNDMdoTT1K6
```

Which gives us something that looks like this

`zpub6jftahH18ngZw98KGjRo5XcxeKTQ2eztsvskb1dC9XF5TLimQquTs6Ry7nBBA425D9joXmfgJJCexmJ1u2SELJZJfRi95gcnXadLpZzYb5c`

which is a p2sh wrapped segwit `ExtPubKey` for the bitcoin main network!

#### Creating a key manager from existing mnemonic

To create a `KeyManager` from existing mnemonic you need to specify the `seedPath` and then construct the `KeyManagerParams` that you would like.

Finally you call `KeyManager.fromParams()` that reads the mnemonic from disk and create's the key manager