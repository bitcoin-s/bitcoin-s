---
title: Wallet Rescans
id: wallet-rescan
---

With [BIP157](https://github.com/bitcoin/bips/blob/master/bip-0157.mediawiki) you can cache block filters locally to use
later for rescans in the case you need to restore your wallets. Our [chain](../applications/chain.md) project gives us
an API with the ability to query for filters.

In combination with a set of scripts, you can iterate through each of the filters with [`WalletApi.rescanNeutrinoWallet()`](https://github.com/bitcoin-s/bitcoin-s/blob/1a3b6b5b1e4eb8442dfab8b1a9faeff74418bdb0/wallet/src/main/scala/org/bitcoins/wallet/api/WalletApi.scala#L399)


#### Example

In this example we are going show how to rescan our bitcoin-s wallet with `CompactFilter`.

The first thing we need to do is spin up a

```scala mdoc:compile-only

```
