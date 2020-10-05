---
id: oracle-server
title: Oracle Server
---

The Oracle Server is a DLC Oracle with functionality for creating events and attesting to them.
It also provides data to be able to set up your staking address.

### Building the Oracle Server

You can build the oracle server with the [sbt native packager](https://github.com/sbt/sbt-native-packager).
The native packager offers [numerous ways to package the project](https://github.com/sbt/sbt-native-packager#examples).

In this example we are going to use `stage` which will produce bash scripts we can easily execute. You can stage the server with the following command.

```bash
sbt oracleServer/universal:stage
```

This will produce a script to execute bitcoin-s which you can start with

```bash
./app/oracle-server/target/universal/stage/bin/bitcoin-s-oracle-server
```

### Configuration

If you would like to pass in a custom datadir for your server, you can do

```bash
./app/oracle-server/target/universal/stage/bin/bitcoin-s-oracle-server --datadir /path/to/datadir/
```

To use a config file that is not the `bitcoin-s.conf` file in your datadir, you can do

```bash
./app/oracle-server/target/universal/stage/bin/bitcoin-s-oracle-server --conf /path/to/file.conf
```

You can also pass in a custom `rpcport` to bind to

```bash
./app/oracle-server/target/universal/stage/bin/bitcoin-s-oracle-server --rpcport 12345
```

For more information on configuring the server please see our [configuration](../config/configuration.md) document

For more information on how to use our built in `cli` to interact with the server please see [cli.md](../applications/cli.md)

### Server Endpoints

- `getpublickey` - Get oracle's public key
- `getstakingaddress` - Get oracle's staking address
- `listevents` - Lists all event nonces
- `createevent` `label` `maturationtime` `outcomes` - Registers an oracle event
  - `label` - Label for this event
  - `maturationtime` - The earliest expected time an outcome will be signed, given in epoch second
  - `outcomes` - Possible outcomes for this event
- `getevent` `nonce` - Get an event's details
  - `nonce` - Nonce associated with the event
- `signevent` `nonce` `outcome` - Signs an event
  - `nonce` - Nonce associated with the event to sign
  - `outcome`- Outcome to sign for this event
- `getsignature` `nonce` - Get the signature from a signed event
  - `nonce` - Nonce associated with the signed event
  
#### Create Event Example

Bitcoin-S CLI:
```bash
$ bitcoin-s-cli createevent testevent 1601917137 "outcome 1,outcome 2,outcome 3"
a777c9fdc42efbbbcb78e51a18ff98cdaafb809aeb082b8ebe95d57b1e21f1aa

$ bitcoin-s-cli getevent a777c9fdc42efbbbcb78e51a18ff98cdaafb809aeb082b8ebe95d57b1e21f1aa
{
  "nonce": "a777c9fdc42efbbbcb78e51a18ff98cdaafb809aeb082b8ebe95d57b1e21f1aa",
  "eventName": "testevent",
  "numOutcomes": 3,
  "signingVersion": "Mock",
  "maturationTime": "2020-10-05T16:58:57Z",
  "announcementSignature": "1533265899006003fa79dc0d480061c3378bc634ec041efc97fc70827f3a1c9f30e05373f36c2e2dd9d2ad64d8aaee17fef724af2284b87724b949d48846920c",
  "attestation": "",
  "signature": "",
  "outcomes": [
    "outcome 1",
    "outcome 2",
    "outcome 3"
  ]
}
```

CURL:
```bash
$ curl --data-binary '{"jsonrpc": "1.0", "id": "curltest", "method": "createevent", "params": ["testEvent", 1601917137, ["outcome 1", "outcome 2", "outcome 3"]]}' -H "Content-Type: application/json" http://127.0.0.1:9999/
{"result":"28592661c78a3c2e0a568e92122e146022cb018b6b0ac888cdffc70a506e9ad2","error":null}

$ curl --data-binary '{"jsonrpc": "1.0", "id": "curltest", "method": "getevent", "params": ["28592661c78a3c2e0a568e92122e146022cb018b6b0ac888cdffc70a506e9ad2"]}' -H "Content-Type: application/json" http://127.0.0.1:9999/
{"result":{"nonce":"28592661c78a3c2e0a568e92122e146022cb018b6b0ac888cdffc70a506e9ad2","eventName":"testEvent","numOutcomes":3,"signingVersion":"Mock","maturationTime":"2020-10-05T16:58:57Z","commitmentSignature":"a91499fa83ca607b06bb919284e002452d6c8f396295495586886b1f7e6d6f094c7d1504f35ee2210a036313569c1951aada6b3d52248f77c7e2c5836a970dd7","attestation":"","signature":"","outcomes":["outcome 1","outcome 2","outcome 3"]},"error":null}
```

#### Sign Event Example

Bitcoin-S CLI:

```bash
$ bitcoin-s-cli signevent a777c9fdc42efbbbcb78e51a18ff98cdaafb809aeb082b8ebe95d57b1e21f1aa "outcome 1"
a777c9fdc42efbbbcb78e51a18ff98cdaafb809aeb082b8ebe95d57b1e21f1aabddd069a3295eb8e02a8a89de4b50b063ffeb290e5d1c6ea3e4e21efb2ad208f

$ bitcoin-s-cli getsignature a777c9fdc42efbbbcb78e51a18ff98cdaafb809aeb082b8ebe95d57b1e21f1aa
a777c9fdc42efbbbcb78e51a18ff98cdaafb809aeb082b8ebe95d57b1e21f1aabddd069a3295eb8e02a8a89de4b50b063ffeb290e5d1c6ea3e4e21efb2ad208f
```

CURL:
```bash
$ curl --data-binary '{"jsonrpc": "1.0", "id": "curltest", "method": "signevent", "params": ["a777c9fdc42efbbbcb78e51a18ff98cdaafb809aeb082b8ebe95d57b1e21f1aa", "outcome 1"]}' -H "Content-Type: application/json" http://127.0.0.1:9999/
{"result":"a777c9fdc42efbbbcb78e51a18ff98cdaafb809aeb082b8ebe95d57b1e21f1aabddd069a3295eb8e02a8a89de4b50b063ffeb290e5d1c6ea3e4e21efb2ad208f","error":null}

$ curl --data-binary '{"jsonrpc": "1.0", "id": "curltest", "method": "getsignature", "params": ["a777c9fdc42efbbbcb78e51a18ff98cdaafb809aeb082b8ebe95d57b1e21f1aa"]}' -H "Content-Type: application/json" http://127.0.0.1:9999/
{"result":"a777c9fdc42efbbbcb78e51a18ff98cdaafb809aeb082b8ebe95d57b1e21f1aabddd069a3295eb8e02a8a89de4b50b063ffeb290e5d1c6ea3e4e21efb2ad208f","error":null}
```
