---
title: Wallet RPC Examples
id: wallet-rpc
---

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
