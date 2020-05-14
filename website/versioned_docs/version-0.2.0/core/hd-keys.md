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
import org.bitcoins.core.crypto._
import org.bitcoins.core.hd._

// the length of the entropy bit vector determine
// how long our phrase ends up being
// 256 bits of entropy results in 24 words
val entropy: BitVector = MnemonicCode.getEntropy256Bits
// entropy: BitVector = BitVector(256 bits, 0x8c462348afe97eef714535da78e85cba3fea90da954d0b4574f43104b0ded45c)

val mnemonicCode = MnemonicCode.fromEntropy(entropy)
// mnemonicCode: MnemonicCode = MnemonicCodeImpl(Vector(middle, country, split, garlic, obey, jeans, shallow, pluck, surprise, shrug, blame, inner, wrong, embody, heart, predict, area, close, dial, ginger, enrich, hunt, fabric, mask))

mnemonicCode.words // the phrase the user should write down
// res0: Vector[String] = Vector(middle, country, split, garlic, obey, jeans, shallow, pluck, surprise, shrug, blame, inner, wrong, embody, heart, predict, area, close, dial, ginger, enrich, hunt, fabric, mask) // the phrase the user should write down

// the password argument is an optional, extra security
// measure. all MnemonicCode instances will give you a
// valid BIP39 seed, but different passwords will give
// you different seeds. So you could have as many wallets
// from the same seed as you'd like, by simply giving them
// different passwords.
val bip39Seed = BIP39Seed.fromMnemonic(mnemonicCode,
                                       password = "secret password")
// bip39Seed: BIP39Seed = BIP39SeedImpl(ByteVector(64 bytes, 0xed5dc8b858bf78427b4fb26645c79e59b777201281549c224b56efbae399846e5b03f284b05bad1499880b10a0592c3ac9a7b01842ced609ec83ccf134e14727))

val xpriv = ExtPrivateKey.fromBIP39Seed(ExtKeyVersion.SegWitMainNetPriv,
                                        bip39Seed)
// xpriv: ExtPrivateKey = zprvAWgYBBk7JR8GkeTo5WZtXUmTSjUXxw8kcA5LdBR8ySCKiXrGfLmCgSbdhrr1RtSjHe19Fc7BACFh5qfArFr5nf7WXaqGLgudpAdG7VzkGM5
val xpub = xpriv.extPublicKey
// xpub: ExtPublicKey = zpub6jftahH18ngZy8YGBY6ttciBzmK2NPrbyNzwRZpkXmjJbLBRCt5TEEv7Z6zRHkkrTR9nYzsUCry97Ru7NVqyMtbRreTo3WWqMdHxzj38dp4

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
// accountXpub: ExtPublicKey = zpub6rFK5YgNZ4VPaRxHLRp5sdC3TeWhEUjxuBm38y4tmH8M4RyA3QuxGEJVuq4YnRVxXdcjj4JajgGV787yGTKZM1SBK2Q1z4ZqJrV6uzewamM

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
// firstAccountAddress: Bech32Address = Bech32Address(tb1qypxkhchu805aprywl8e4eezfqkkkwa39gj2qeu)

// tada! We just generated an address you can send money to,
// without having access to the private key!
firstAccountAddress.value
// res2: String = tb1qypxkhchu805aprywl8e4eezfqkkkwa39gj2qeu

// you can now continue deriving addresses from the same public
// key, by imitating what we did above. To get the next
// HD path to generate an address at:
val nextAddressPath: SegWitHDPath = firstAddressPath.next
// nextAddressPath: SegWitHDPath = m/84'/0'/0'/0/1
```

### Signing things with HD keys

Please see [sign.md](sign.md) for information on how to sign things with HD keys.