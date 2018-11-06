[ ![Download](https://api.bintray.com/packages/bitcoin-s/bitcoin-s-core/bitcoin-s-bitcoind-rpc/images/download.svg) ](https://bintray.com/bitcoin-s/bitcoin-s-core/bitcoin-s-bitcoind-rpc/_latestVersion)

> Note: `bitcoin-s-bitcoind-rpc` requires you to have `bitcoind` (Bitcoin Core daemon) installed. Grab this at [bitcoincore.org](https://bitcoincore.org/en/download/)

## Testing

To test the Bitcoin-S RPC project you need both version 0.16 and 0.17 of Bitcoin Core. A list of current and previous releases can be found [here](https://bitcoincore.org/en/releases/).

You then need to set environment variables to indicate where Bitcoin-S can find the different versions: 

```bash
$ export BITCOIND_V16_PATH=/path/to/v16/bitcoind
$ export BITCOIND_V17_PATH=/path/to/v17/bitcoind
```

If you just run tests testing common functionality it's enough to have either version 0.16 or 0.17 on your `PATH`.

To run all RPC related tests:

```bash
$ bash sbt bitcoindRpcTest/test
```

