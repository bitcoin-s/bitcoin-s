---
title: Wallet RPC Examples
id: version-1.7.0-wallet-rpc
original_id: wallet-rpc
---

### `listreservedutxos`

Lists all reserved utxos in the wallet.
These utxos will not be unreserved unless you manually
unreserve them with `lockunspent` or they are spent in the blockchain

```bash
bitcoin-s-cli listreservedutxos
[
  {
    "outpoint": {
        "txid": "1c22634fa282e71866a8b8c6732ec89eb5c46d30f9773486b0ae32770e8109e1",
        "vout": 1,
    },
    "value": 2000
  },
  {
    "outpoint": {
        "txid": "2b12634fa282e71866a8b8c6732ec89eb5c46d30f9773486b0ae32770e810901",
        "vout": 0,
    },
    "value": 1000
  }
]

```


### `lockunspent`

Locks all utxos in the wallet
```bash
bitcoin-s-cli lockunspent false
```

Unlocks utxo `1c22634fa282e71866a8b8c6732ec89eb5c46d30f9773486b0ae32770e8109e1:1`
```bash
bitcoin-s-cli lockunspent true '{"txid" : "1c22634fa282e71866a8b8c6732ec89eb5c46d30f9773486b0ae32770e8109e1","vout" : 1}'
```

Locks utxos `1c22634fa282e71866a8b8c6732ec89eb5c46d30f9773486b0ae32770e8109e1:1` and `4c63268a688d103caeb26137cecd4053566bd3626504e079055581c104c4de5b:0`
```bash
bitcoin-s-cli lockunspent false '[{"txid" : "1c22634fa282e71866a8b8c6732ec89eb5c46d30f9773486b0ae32770e8109e1","vout" : 1}, {"txid" : "4c63268a688d103caeb26137cecd4053566bd3626504e079055581c104c4de5b","vout" : 0}]'
```
