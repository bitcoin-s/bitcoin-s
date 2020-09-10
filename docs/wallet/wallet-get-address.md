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

## Address Queue

The Bitcoin-S wallet uses a background thread meant to ensure safety when fetching addresses.
This is to ensure independent calls to getNewAddress don't result in a race condition to the database that would generate the same address and cause an error.
With this background thread, we poll the `addressRequestQueue` seeing if there are any elements in it, if there are, we process them and complete the Promise in the queue.

There are two ways to configure the wallet's address queue.

1. `addressQueueSize`: How big the address queue size is before we throw an overflow exception

2. `addressQueueTimeout`: How long we attempt to generate an address for before it times out

For an example configuration, checkout the [example config](../config/configuration.md#example-configuration-file).
