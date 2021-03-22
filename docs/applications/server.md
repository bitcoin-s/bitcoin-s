---
id: server
title: Application Server
---


## App server

The server project is the aggregation of these three sub projects

1. [Wallet](../wallet/wallet.md)
2. [Chain](../chain/chain.md)
3. [Node](../node/node.md)

The server project provides a away to access information from these three projects via a JSON RPC.

## Building the server

### Java binary
You can build the server with the [sbt native packager](https://github.com/sbt/sbt-native-packager).
The native packager offers [numerous ways to package the project](https://github.com/sbt/sbt-native-packager#examples).

In this example we are going to use `stage` which will produce bash scripts we can easily execute. You can stage the server with the following command.

```bash
sbt appServer/universal:stage
```

This will produce a script to execute bitcoin-s which you can start with

```bash
./app/server/target/universal/stage/bin/bitcoin-s-server
```

### Docker

The oracle server also has docker support. You can build a docker image with the following commands

#### Using an existing docker image

We publish docker images on every PR that is merged to bitcoin-s.

You can find the docker repo for the app server [here](https://hub.docker.com/r/bitcoinscala/bitcoin-s-server/tags?page=1&ordering=last_updated)

#### Building a docker image
```
sbt "appServer/docker:stage"
```

This will build a `Dockerfile` that is located in `app/server/target/docker/stage`

You can now build the docker image with

```
docker build app/server/target/docker/stage/ -t bitcoin-s-server:latest
```

Finally, let's run the image! It's important that you correctly configure port forwarding with the docker container so
you can interact with the running container with `bitcoin-s-cli` or `curl`. By default, our oracle
server listens for requests on port `9999`.

This means we need to forward requests on the host machine to the docker container correctly.

This can be done with the following command
```
docker run -d -p 9999:9999 bitcoin-s-server:latest:latest
```

Now you can send requests with `bitcoin-s-cli` or `curl`.
Here is an example with `bitcoin-s-cli`
```
./bitcoin-s-cli getblockcount
10000
```

For more information on build configuration options with `sbt` please see the [sbt native packager docs](https://sbt-native-packager.readthedocs.io/en/latest/formats/docker.html#tasks)

## Configuration

### Java binary configuration
If you would like to pass in a custom datadir for your server, you can do

```bash
./app/server/target/universal/stage/bin/bitcoin-s-server --datadir /path/to/datadir/
```

To use a config file that is not the `bitcoin-s.conf` file in your datadir, you can do

```bash
./app/server/target/universal/stage/bin/bitcoin-s-server --conf /path/to/file.conf
```

You can also pass in a custom `rpcport` to bind to

```bash
./app/server/target/universal/stage/bin/bitcoin-s-server --rpcport 12345
```

For more information on configuring the server please see our [configuration](../config/configuration.md) document

For more information on how to use our built in `cli` to interact with the server please see [cli.md](cli.md)

### Docker configuration

In this example, we are using the latest docker image published to our [docker hub](https://hub.docker.com/repository/docker/bitcoinscala/bitcoin-s-oracle-server/tags?page=1&ordering=last_updated)
which is referenced by `bitcoinscala/bitcoin-s-server:latest`

You can use bitcoin-s with docker volumes. You can also pass in a custom configuration at container runtime.

#### Using a docker volume

```basrc
docker volume create bitcoin-s
docker run -p 9999:9999 \
--mount source=bitcoin-s,target=/home/bitcoin-s/ bitcoinscala/bitcoin-s-server:latest
```

Now you can re-use this volume across container runs. It will keep the same oracle database
and seeds directory located at `/home/bitcoin-s/.bitcoin-s/seeds` in the volume.

#### Using a custom bitcoin-s configuration with docker

You can also specify a custom bitcoin-s configuration at container runtime.
You can mount the configuration file on the docker container and that
configuration will be used in the docker container runtime rather than
the default one we provide [here](https://github.com/bitcoin-s/bitcoin-s/blob/master/app/oracle-server/src/universal/docker-application.conf)

You can do this with the following command

```bashrc
docker run -p 9999:9999 \
--mount type=bind,source=/my/new/config/,target=/home/bitcoin-s/.bitcoin-s/ \
bitcoinscala/bitcoin-s-server:latest --conf /home/bitcoin-s/.bitcoin-s/bitcoin-s.conf
```

Note: If you adjust the `bitcoin-s.server.rpcport` setting you will need to adjust
the `-p 9999:9999` port mapping on the docker container to adjust for this.

## Server Endpoints

### Blockchain

 - `getblockcount` - Get the current block height
 - `getfiltercount` - Get the number of filters
 - `getfilterheadercount` - Get the number of filter headers
 - `getbestblockhash` - Get the best block hash
 - `getblockheader` - Returns information about block header <hash>
     - `hash` - The block hash
 - `decoderawtransaction` `tx` - `Decode the given raw hex transaction`
     - `tx` - Transaction encoded in hex to decode

### Wallet
 - `rescan` `[options]` - Rescan for wallet UTXOs
    - `--force` - Clears existing wallet records. Warning! Use with caution!
    - `--batch-size <value>` - Number of filters that can be matched in one batch
    - `--start <value>` - Start height
    - `--end <value>` - End height
    - `--ignorecreationtime` - Ignores the wallet creation date and will instead do a full rescan
 - `isempty` - Checks if the wallet contains any data
 - `walletinfo` - Returns data about the current wallet being used
 - `getbalance` `[options]` - Get the wallet balance
    - `--sats ` - Display balance in satoshis
 - `getconfirmedbalance` `[options]` - Get the wallet balance of confirmed utxos
    - `--sats ` - Display balance in satoshis
 - `getunconfirmedbalance` `[options]` - Get the wallet balance of unconfirmed utxos
    - `--sats ` - Display balance in satoshis
 - `getutxos` - Returns list of all wallet utxos
 - `getaddresses` - Returns list of all wallet addresses currently being watched
 - `getspentaddresses` - Returns list of all wallet addresses that have received funds and been spent
 - `getfundedaddresses` - Returns list of all wallet addresses that are holding funds
 - `getunusedaddresses` - Returns list of all wallet addresses that have not been used
 - `getaccounts` - Returns list of all wallet accounts
 - `walletinfo` - Returns meta information about the wallet
 - `createnewaccount` - Creates a new wallet account
 - `getaddressinfo` `address` - Returns list of all wallet accounts
    - `address` - Address to get information about
 - `getnewaddress` - Get a new address
 - `sendtoaddress` `address` `amount` `[options]` - Send money to the given address
    - `address` - Address to send to
    - `amount` - Amount to send in BTC
    - `--feerate <value>` - Fee rate in sats per virtual byte
 - `sendfromoutpoints` `outpoints` `address` `amount` `[options]` - Send money to the given address
    - `outpoints` - Out Points to send from
    - `address` - Address to send to
    - `amount` - Amount to send in BTC
    - `--feerate <value>` - Fee rate in sats per virtual byte
 - `sendwithalgo` `address` `amount` `algo` `[options]` - Send money to the given address using a specific coin selection algo
    - `address` - Address to send to
    - `amount` - Amount to send in BTC
    - `algo` - Coin selection algo
    - `--feerate <value>` - Fee rate in sats per virtual byte
 - `signpsbt` `psbt` - Signs the PSBT's inputs with keys that are associated with the wallet
    - `psbt` - PSBT to sign
 - `opreturncommit` `message` `[options]` - Creates OP_RETURN commitment transaction
    - `message` - message to put into OP_RETURN commitment
    - `--hashMessage` - should the message be hashed before commitment
    - `--feerate <value>` - Fee rate in sats per virtual byte
 - `bumpfeecpfp` `txid` `feerate` - Bump the fee of the given transaction id with a child tx using the given fee rate
    - `txid` - Id of transaction to bump fee
    - `feerate` - Fee rate in sats per virtual byte of the child transaction
 - `bumpfeerbf` `txid` `feerate` - Replace given transaction with one with the new fee rate
    - `txid` - Id of transaction to bump fee
    - `feerate` - New fee rate in sats per virtual byte
 - `gettransaction` `txid` - Get detailed information about in-wallet transaction <txid>
    - `txid` - The transaction id
 - `lockunspent` `unlock` `transactions` - Temporarily lock (unlock=false) or unlock (unlock=true) specified transaction outputs.
    - `unlock` - Whether to unlock (true) or lock (false) the specified transactions
    - `transactions` - The transaction outpoints to unlock/lock
- `importseed` `walletname` `words` `passphrase` - Imports a mnemonic seed as a new seed file
   - `walletname` - Name to associate with this seed
   - `words` - Mnemonic seed words, space separated
   - `passphrase` - Passphrase to encrypt this seed with
- `importxprv` `walletname` `xprv` `passphrase` - Imports a mnemonic seed as a new seed file
   - `walletname` - Name to associate with this seed
   - `xprv` - base58 encoded extended private key
   - `passphrase` - Passphrase to encrypt this seed with
 - `keymanagerpassphrasechange` `oldpassphrase` `newpassphrase` - Changes the wallet passphrase
    - `oldpassphrase` - The current passphrase
    - `newpassphrase` - The new passphrase
 - `keymanagerpassphraseset` `passphrase` - Encrypts the wallet with the given passphrase
    - `passphrase` - The passphrase to encrypt the wallet with

### Network
 - `getpeers` - List the connected peers
 - `stop` - Request a graceful shutdown of Bitcoin-S
 - `sendrawtransaction` `tx` `Broadcasts the raw transaction`
    - `tx` - Transaction serialized in hex

### PSBT
 - `decodepsbt` `psbt` - Return a JSON object representing the serialized, base64-encoded partially signed Bitcoin transaction.
    - `psbt` - PSBT serialized in hex or base64 format
 - `combinepsbts` `psbts` - Combines all the given PSBTs
    - `psbts` - PSBTs serialized in hex or base64 format
 - `joinpsbts` `psbts` - Combines all the given PSBTs
    - `psbts` - PSBTs serialized in hex or base64 format
 - `finalizepsbt` `psbt` - Finalizes the given PSBT if it can
    - `psbt` - PSBT serialized in hex or base64 format
 - `extractfrompsbt` `psbt` - Extracts a transaction from the given PSBT if it can
    - `psbt` - PSBT serialized in hex or base64 format
 - `converttopsbt` `unsignedTx` - Creates an empty psbt from the given transaction
    - `unsignedTx` - serialized unsigned transaction in hex

### Util
 - `createmultisig` `nrequired` `keys` `[address_type]` - Creates a multi-signature address with n signature of m keys required.
    - `nrequired` - The number of required signatures out of the n keys.
    - `keys` - The hex-encoded public keys.
    - `address_type` -The address type to use. Options are "legacy", "p2sh-segwit", and "bech32"

## Sign PSBT with Wallet Example

Bitcoin-S CLI:

```bash
$ bitcoin-s-cli signpsbt cHNidP8BAP0FAQIAAAABWUWxYiPKgdGfXcIxJ6MRDxEpUecw59Gk4NpROI5oukoBAAAAAAAAAAAEPttkvdwAAAAXqRSOVAp6Qe/u2hq74e/ThB8foBKn7IfZYMgGCAAAAADbmaQ2nwAAAEdRIQLpfVqyaL9Jb/IkveatNyVeONE8Q/6TzXAWosxLo9e21SECc5G3XiK7xKLlkBG7prMx7p0fMeQwMH5e9H10mBon39JSrtgtgjjLAQAAUGMhAn2YaZnv25I6d6vbb1kw6Xp5IToDrEzl/0VBIW21gHrTZwXg5jGdALJ1IQKyNpDNiOiN6lWpYethib04+XC9bpFXrdpec+xO3U5IM2is9ckf5AABAD0CAAAAAALuiOL0rRcAABYAFPnpLByQq1Gg3vwiP6qR8FmOOjwxvVllM08DAAALBfXJH+QAsXUAAK4AAAAAAQcBAAAAAAAA
cHNidP8BAP0FAQIAAAABWUWxYiPKgdGfXcIxJ6MRDxEpUecw59Gk4NpROI5oukoBAAAAAAAAAAAEPttkvdwAAAAXqRSOVAp6Qe/u2hq74e/ThB8foBKn7IfZYMgGCAAAAADbmaQ2nwAAAEdRIQLpfVqyaL9Jb/IkveatNyVeONE8Q/6TzXAWosxLo9e21SECc5G3XiK7xKLlkBG7prMx7p0fMeQwMH5e9H10mBon39JSrtgtgjjLAQAAUGMhAn2YaZnv25I6d6vbb1kw6Xp5IToDrEzl/0VBIW21gHrTZwXg5jGdALJ1IQKyNpDNiOiN6lWpYethib04+XC9bpFXrdpec+xO3U5IM2is9ckf5AABAD0CAAAAAALuiOL0rRcAABYAFPnpLByQq1Gg3vwiP6qR8FmOOjwxvVllM08DAAALBfXJH+QAsXUAAK4AAAAAAQcBAAAAAAAA
```

CURL:
```bash
$ curl --data-binary '{"jsonrpc": "1.0", "id": "curltest", "method": "signpsbt", "params": ["cHNidP8BAP0FAQIAAAABWUWxYiPKgdGfXcIxJ6MRDxEpUecw59Gk4NpROI5oukoBAAAAAAAAAAAEPttkvdwAAAAXqRSOVAp6Qe/u2hq74e/ThB8foBKn7IfZYMgGCAAAAADbmaQ2nwAAAEdRIQLpfVqyaL9Jb/IkveatNyVeONE8Q/6TzXAWosxLo9e21SECc5G3XiK7xKLlkBG7prMx7p0fMeQwMH5e9H10mBon39JSrtgtgjjLAQAAUGMhAn2YaZnv25I6d6vbb1kw6Xp5IToDrEzl/0VBIW21gHrTZwXg5jGdALJ1IQKyNpDNiOiN6lWpYethib04+XC9bpFXrdpec+xO3U5IM2is9ckf5AABAD0CAAAAAALuiOL0rRcAABYAFPnpLByQq1Gg3vwiP6qR8FmOOjwxvVllM08DAAALBfXJH+QAsXUAAK4AAAAAAQcBAAAAAAAA"]}' -H "Content-Type: application/json" http://127.0.0.1:9999/
{"result":"cHNidP8BAP0FAQIAAAABWUWxYiPKgdGfXcIxJ6MRDxEpUecw59Gk4NpROI5oukoBAAAAAAAAAAAEPttkvdwAAAAXqRSOVAp6Qe/u2hq74e/ThB8foBKn7IfZYMgGCAAAAADbmaQ2nwAAAEdRIQLpfVqyaL9Jb/IkveatNyVeONE8Q/6TzXAWosxLo9e21SECc5G3XiK7xKLlkBG7prMx7p0fMeQwMH5e9H10mBon39JSrtgtgjjLAQAAUGMhAn2YaZnv25I6d6vbb1kw6Xp5IToDrEzl/0VBIW21gHrTZwXg5jGdALJ1IQKyNpDNiOiN6lWpYethib04+XC9bpFXrdpec+xO3U5IM2is9ckf5AABAD0CAAAAAALuiOL0rRcAABYAFPnpLByQq1Gg3vwiP6qR8FmOOjwxvVllM08DAAALBfXJH+QAsXUAAK4AAAAAAQcBAAAAAAAA","error":null}
```
