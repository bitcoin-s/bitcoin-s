See the `bitcoind`/Bitcoin Core section on the 
Bitcoin-S [website](https://bitcoin-s.org/docs/rpc/rpc-bitcoind).

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
$ bloop test bitcoindRpcTest
```
