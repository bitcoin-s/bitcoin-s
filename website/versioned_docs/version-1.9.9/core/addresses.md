---
id: version-1.9.9-addresses
title: Generating Addresses
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
// this generates a random private key
val privkey = ECPrivateKey()
// privkey: ECPrivateKey = Masked(ECPrivateKey)
val pubkey = privkey.publicKey
// pubkey: org.bitcoins.crypto.ECPublicKey = ECPublicKey(02e2fa471bab2b8aeffc6090485db8c47be5bc86eb6d8d720ed6338d9b3c8387dd)

val segwitAddress = {
    // see https://bitcoin.org/en/glossary/pubkey-script
    // for reading resources on the details of scriptPubKeys
    // pay-to-witness-pubkey-hash scriptPubKey V0
    val scriptPubKey = P2WPKHWitnessSPKV0(pubkey)
    Bech32Address(scriptPubKey, TestNet3)
}
// segwitAddress: Bech32Address = tb1qlq2ev2hn5egsgzq2eeu7y4w3dvngahc0mzul60

println(segwitAddress.toString)
// tb1qlq2ev2hn5egsgzq2eeu7y4w3dvngahc0mzul60
```

## Generating legacy (base58) addresses

If you need to generate legacy addresses for backwards
compatability reasons, that's also a walk in the park.
Take a look:

```scala
// we're reusing the same private/public key pair
// from before. don't do this in an actual application!
val legacyAddress = P2PKHAddress(pubkey, TestNet3)
// legacyAddress: P2PKHAddress = n48hjVR5fseSoNPbE95Zw36PSTbTvfWWf9

println(legacyAddress.toString)
// n48hjVR5fseSoNPbE95Zw36PSTbTvfWWf9
```
