---
title: Wallet Get Address APIs
id: wallet-get-address
---

The Bitcoin-S wallet has a few different functions for getting a wallet address
that all server different purposes.

First, there is `getNewAddress`. This function will always return a new a address
determined by the next address available address index.

Second, we have `getUnusedAddress`. This function will return an address that has
not ever received funds.

Third, there is `getAddress`. This function takes in an `AccountDb`, `HDChainType`,
and an `Int`. This will return the address associated with the pubkey at
the resulting `BIP32Path`.
