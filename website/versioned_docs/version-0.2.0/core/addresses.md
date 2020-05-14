---
id: version-0.2.0-addresses
title: Generating addresses
original_id: addresses
---

Almost all Bitcoin applications need to generate addresses
for their users somehow. There's a lot going on in getting
a correct bitcoin address, but our APIs make it possible to
to get started with all types of addresses in a matter of
minutes.

## Generating SegWit (bech32) addresses

Generating native SegWit addresses in the bech32 format
is something that all Bitcoin applications should enable,
as it makes the transaction fees less expensive, and also
makes the addresses more readable by humans. However, it
has seen slower than necessary adoption. With Bitcoin-S
you can generate bech32 addresses in four(!) lines of code
(not counting comments and imports), so now there's no
reason to keep using legacy transaction formats.

```scala

// if you want to get addresses for mainnet, just import
// config.MainNet here instead
import config.TestNet3

// this gets all addresses into scope
import protocol._

// this gets all scriptPubKeys into scope
import protocol.script._

// this generates a random private key
val privkey = ECPrivateKey()
// privkey: ECPrivateKey = ECPrivateKey(6e7b7ba3c192ca7ecc3681f7eb1e021c9f1333a45c7ff88cace595ee5759aa69,true)
val pubkey = privkey.publicKey
// pubkey: crypto.ECPublicKey = ECPublicKey(0225ae3b07de8578ba6a9988872028f79d9cb46d703a39491dc8f91a74b93b0a8c)

val segwitAddress = {
    // see https://bitcoin.org/en/glossary/pubkey-script
    // for reading resources on the details of scriptPubKeys
    // pay-to-witness-pubkey-hash scriptPubKey V0
    val scriptPubKey = P2WPKHWitnessSPKV0(pubkey)
    Bech32Address(scriptPubKey, TestNet3)
}
// segwitAddress: Bech32Address = Bech32Address(tb1qydaft7sx02025te2e3hu4np87d65xg33ghzcwe)
```

## Generating legacy (base58) addresses

If you need to generate legacy addresses for backwards
compatability reasons, that's also a walk in the park.
Take a look:

```scala
// pay-to-pubkey-hash address
import org.bitcoins.core.protocol.P2PKHAddress

// we're reusing the same private/public key pair
// from before. don't do this in an actual application!
val legacyAddress = P2PKHAddress(pubkey, TestNet3)
// legacyAddress: P2PKHAddress = mikYoBWXrLKpagxwNLgMUyUVv6TEXvxDgz
```
