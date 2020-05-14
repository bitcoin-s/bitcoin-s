---
id: version-0.1.0-addresses
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
// privkey: ECPrivateKey = ECPrivateKey(9a56486eeeef146555ee6dcde9039d205649d896155855b44c8296c0464dfdb0,true)
val pubkey = privkey.publicKey
// pubkey: crypto.ECPublicKey = ECPublicKey(0367b1156e1c11d69de16ff76b2c4b249396f9bb86cf5933837e147638cdae9485)

val segwitAddress = {
    // see https://bitcoin.org/en/glossary/pubkey-script
    // for reading resources on the details of scriptPubKeys
    // pay-to-witness-pubkey-hash scriptPubKey V0
    val scriptPubKey = P2WPKHWitnessSPKV0(pubkey)
    Bech32Address(scriptPubKey, TestNet3)
}
// segwitAddress: Bech32Address = Bech32Address(tb1qde6457araspjqa4hjkqfsm8z3jax97exte40h9)
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
// legacyAddress: P2PKHAddress = mqb1D1swAkQEAsXbznjis6CWcKp7241cM8
```

