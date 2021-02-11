---
id: version-0.5.0-hd-keys
title: HD Key Generation
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
// entropy: BitVector = BitVector(256 bits, 0xb39105335b179bc75b1b3bc2f98a72558824b836d30dc2f827a0deff62263933)

val mnemonicCode = MnemonicCode.fromEntropy(entropy)
// mnemonicCode: MnemonicCode = Masked(MnemonicCodeImpl)

mnemonicCode.words // the phrase the user should write down
// res0: Vector[String] = Vector(receive, market, error, renew, keen, together, hockey, guess, seed, slush, orient, prison, liquid, foster, swap, giant, seed, scorpion, trial, sadness, wage, basic, tooth, husband) // the phrase the user should write down

// the password argument is an optional, extra security
// measure. all MnemonicCode instances will give you a
// valid BIP39 seed, but different passwords will give
// you different seeds. So you could have as many wallets
// from the same seed as you'd like, by simply giving them
// different passwords.
val bip39Seed = BIP39Seed.fromMnemonic(mnemonicCode,
                                       password = "secret password")
// bip39Seed: BIP39Seed = Masked(BIP39SeedImpl)

val xpriv = ExtPrivateKey.fromBIP39Seed(ExtKeyVersion.SegWitMainNetPriv,
                                        bip39Seed)
// xpriv: ExtPrivateKey = Masked(ExtPrivateKeyImpl)
val xpub = xpriv.extPublicKey
// xpub: ExtPublicKey = zpub6jftahH18ngZxm7szuMGGg1mZxk46iojb4ya9Smp9rNXzqj6EpBRHBTxVPDPfz9ihomPxpQqkSRbVBJDwY17V37uNiMYA5WpCDwCPi2RgHi

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
// accountXpub: ExtPublicKey = zpub6rsUPkH6CLypi8pcGf1Uu9DZCiWH8SMt4X6biYGbQHhaCX5BGBm9mA8fPRS64dGuXqgQBnGATUSPStG7RFAZgeakrrkH2irxBqh9QSrzK1b

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
// firstAccountAddress: Bech32Address = tb1qccq4vzhchs23sjf9xgjlv7jyut5d9nqhhnnwh2

// tada! We just generated an address you can send money to,
// without having access to the private key!
firstAccountAddress.value
// res2: String = tb1qccq4vzhchs23sjf9xgjlv7jyut5d9nqhhnnwh2

// you can now continue deriving addresses from the same public
// key, by imitating what we did above. To get the next
// HD path to generate an address at:
val nextAddressPath: SegWitHDPath = firstAddressPath.next
// nextAddressPath: SegWitHDPath = m/84'/0'/0'/0/1
```

### Signing things with HD keys

Please see [sign.md](../crypto/sign.md) for information on how to sign things with HD keys.