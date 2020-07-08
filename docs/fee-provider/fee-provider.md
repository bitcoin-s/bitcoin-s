---
id: fee-provider
title: Fee Provider
---

# Fee Provider

Bitcoin-S has a `FeeProvider` that is used to fetch fee rates.
Currently, Bitcoin-s has a couple implemented, one being a `BitcoindRpcClient`, which will use `estimateSmartFee` to calculate a fee rate.
Another uses [bitcoiner.live's api](https://bitcoiner.live/) to get a fee rate.

Any `FeeProvider` can be passed to a `Wallet` which will be used to calculate fees for transactions when one is not specified.

## HttpFeeRateProvider

A `HttpFeeRateProvider` is a `FeeProvider` that uses an outside API to get fee rates.
These can be hooked up to any website's API as long as you can provide a `URI` and a function to convert the response to a `FeeUnit`.

There also exists `CachedHttpFeeRateProvider`, which will cache the response for the `cacheDuration` as to prevent hitting request limits and save on api calls.

Checkout [`BitcoinerLiveFeeRateProvider`'s implementation](https://github.com/bitcoin-s/bitcoin-s/blob/master/fee-provider/src/main/scala/org/bitcoins/feeprovider/BitcoinerLiveFeeRateProvider.scala) for an example.
