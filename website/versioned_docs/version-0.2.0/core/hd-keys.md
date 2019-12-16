---
id: version-0.2.0-hd-keys
title: HD key generation
original_id: hd-keys
---

In modern Bitcoin wallets, users only need to write down
a sequence of words, and that sequence is a complete backup
of their wallet. This is thanks to what's called Hierarchical
Deterministic key generation. In short, every wallet using HD
key generation has a root seed for each wallet, and this
seed can be used to generate an arbitrary amount of later
private and public keys. This is done in a standardized manner,
so different wallets can operate with the same standard.

> If you want to jump into the details of how this work,
> you should check out
> [BIP 32](https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki).

Bitcoin-S supports generating keys in this fashion. Here's a
full example of how to obtain a wallet seed, and then
use that to generate further private and public keys:

```scala
import scodec.bits._
import org.bitcoins.core.crypto._
import org.bitcoins.core.hd._

// the length of the entropy bit vector determine
// how long our phrase ends up being
// 256 bits of entropy results in 24 words
val entropy: BitVector = MnemonicCode.getEntropy256Bits
// entropy: BitVector = BitVector(256 bits, 0x39f30e63d03882325006e97c5a15e6d471a4c7d35112569d2352b052a091e3cf)

val mnemonicCode = MnemonicCode.fromEntropy(entropy)
// mnemonicCode: MnemonicCode = MnemonicCodeImpl(Vector(delay, observe, ocean, parrot, market, bomb, divorce, tag, labor, spawn, keen, pottery, bottom, glue, essence, car, final, trouble, start, gaze, claw, employ, monkey, type))

mnemonicCode.words // the phrase the user should write down
// res0: Vector[String] = Vector(delay, observe, ocean, parrot, market, bomb, divorce, tag, labor, spawn, keen, pottery, bottom, glue, essence, car, final, trouble, start, gaze, claw, employ, monkey, type) // the phrase the user should write down

// the password argument is an optional, extra security
// measure. all MnemonicCode instances will give you a
// valid BIP39 seed, but different passwords will give
// you different seeds. So you could have as many wallets
// from the same seed as you'd like, by simply giving them
// different passwords.
val bip39Seed = BIP39Seed.fromMnemonic(mnemonicCode,
                                       password = "secret password")
// bip39Seed: BIP39Seed = BIP39SeedImpl(ByteVector(64 bytes, 0xd45cc7ef7aa2aae1a8cd90c91a4f79b866dd00a10cf8cdaae157a822167a4cc740b7364eb43f1474d8c86473db4faf845e6a9e3fd06ee39eeb493c751a0cdfee))

val xpriv = ExtPrivateKey.fromBIP39Seed(ExtKeyVersion.SegWitMainNetPriv,
                                        bip39Seed)
// xpriv: ExtPrivateKey = zprvAWgYBBk7JR8GjukHugLbrUkjRE3ycvmhRCT4cE2T2qU53j18nUjL4ocaDsXwBWYzU5u88ZFxQ42YWMLAT1S5V2wwcxgogsMrDQtNeLBwr3D
val xpub = xpriv.extPublicKey
// xpub: ExtPublicKey = zpub6jftahH18ngZxPpm1hscDchTyFtU2PVYnRNfQcS4bB13vXLHL23acbw459g4XxYYHfo44rTmYSXzBQzzBEVqT3PfPcqL69fpdrgkxMHuQCy

// you can now use the generated xpriv to derive further
// private or public keys

// this can be done with BIP89 paths (called SegWitHDPath in bitcoin-s)
val segwitPath = SegWitHDPath.fromString("m/84'/0'/0'/0/0")
// segwitPath: SegWitHDPath = m/84'/0'/0'/0/0

// alternatively:
val otherSegwitPath =
SegWitHDPath(HDCoinType.Bitcoin,
             accountIndex = 0,
             HDChainType.External,
             addressIndex = 0)
// otherSegwitPath: SegWitHDPath = m/84'/0'/0'/0/0

segwitPath == otherSegwitPath
// res1: Boolean = true
```

## Generating new addresses without having access to the private key

One the coolest features of HD wallets is that it's possible
to generate addresses offline, without having access to the
private keys. This feature is commonly called watch-only
wallets, where a wallet can import information about all
your past and future transactions, without being able to
spend or steal any of your money.

Let's see an example of this:

```scala
import scala.util.Success
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.Bech32Address
import org.bitcoins.core.config.TestNet3

//                          first account -------┐
//                          bitcoin ----------┐  |
//                          segwit --------┐  |  |
val accountPath = BIP32Path.fromString("m/84'/0'/0'")
// accountPath: BIP32Path = m/84'/0'/0'
val accountXpub = {
    // this key is sensitive, keep away from prying eyes!
    val accountXpriv = xpriv.deriveChildPrivKey(accountPath)

    // this key is not sufficient to spend from, but we
    // can generate addresses with it!
    accountXpriv.extPublicKey
}
// accountXpub: ExtPublicKey = zpub6qUM2b9Nq9v51X1sPsXpXWznJnBhvhHUfhrXvMFntXQT8ZkK2tizi29cMm2HH3vb1Zz8CHb3933J9tvaH97GS4s6XUZQsrTYf7cWGYtmYLA

                              // address no. 0 ---------------┐
                              // external address ----------┐ |
val firstAddressPath = SegWitHDPath.fromString("m/84'/0'/0'/0/0")
// firstAddressPath: SegWitHDPath = m/84'/0'/0'/0/0
val firstAccountAddress = {
    // this is a bit quirky, but we're not interesting in
    // deriving the complete path from our account xpub
    // instead, we're only interested in the part after
    // the account level (3rd level). the .diff() method
    // achieves that
    val Some(pathDiff) = accountPath.diff(firstAddressPath)

    // deriving public keys from hardened extended keys
    // is not possible, that's why .deriveChildPubKey()
    // returns a Try[ExtPublicKey]. A hardened key is marked
    // by a ' after the number in the notation we use above.
    val Success(extPubKey) = accountXpub.deriveChildPubKey(pathDiff)
    val pubkey = extPubKey.key
    val scriptPubKey = P2WPKHWitnessSPKV0(pubkey)
    Bech32Address(scriptPubKey, TestNet3)
}
// firstAccountAddress: Bech32Address = Bech32Address(tb1quqvdsqdzt2nlszkfqsxndu5pg6ahzyjk2e5qmz)

// tada! We just generated an address you can send money to,
// without having access to the private key!
firstAccountAddress.value
// res2: String = tb1quqvdsqdzt2nlszkfqsxndu5pg6ahzyjk2e5qmz

// you can now continue deriving addresses from the same public
// key, by imitating what we did above. To get the next
// HD path to generate an address at:
val nextAddressPath: SegWitHDPath = firstAddressPath.next
// nextAddressPath: SegWitHDPath = m/84'/0'/0'/0/1
```
