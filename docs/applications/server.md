---
id: server
title: Application Server
---


### App server

The server project is the aggregation of these three sub projects

1. [Wallet](../wallet/wallet.md)
2. [Chain](../chain/chain.md)
3. [Node](../node/node.md)

The server project provides a away to access information from these three projects via a JSON RPC.

### Building the server

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

### Installation

#### Linux 

For server side installation you'll need to install a Java Virtual Machine first.

```bash
sudo apt install openjdk-11-jdk-headless
```

Then build a Bitcoin-S server as described above and copy it into `/usr/local`:

```bash
sudo cp -r app/server/target/universal/stage /usr/local/bitcoin-s
sudo chmod +x /usr/local/bitcoin-s/bin/bitcoin-s-server
```

The server process will run in the background and use a separate user for security reasons. 
This user does not have admin rights and cannot change the system configuration.

```bash
sudo adduser bitcoins
```

In this case you'll need to put the config file into `/home/bitcoins/.bitcoin-s/bitcoin-s.conf`.  

To start the server as a daemon on system startup we'll need to configure a `systemd` service. 
Create `bitcoin-s.service` file using your favorite text editor.

```bash
sudo nano /etc/systemd/system/bitcoin-s.service
```

Then copy this script into the editor, then save end exit.  


```bash
[Unit]
Description=Bitcoin-S Node
After=network.target

[Service]
ExecStart=/usr/local/bitcoin-s/bin/bitcoin-s-server

User=bitcoins
Group=bitcoins

Type=simple
Restart=always
RestartSec=60

PrivateTmp=true
ProtectSystem=full
NoNewPrivileges=true
PrivateDevices=true

[Install]
WantedBy=multi-user.target
```

Enable the service:

```bash
sudo systemctl enable bitcoin-s.service
```

Start the server.

```bash
sudo systemctl start bitcoin-s.service
```

The server will write all logs into `/var/log/syslog`.

### Configuration

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

### Server Endpoints

#### Blockchain

 - `getblockcount` - Get the current block height
 - `getfiltercount` - Get the number of filters
 - `getfilterheadercount` - Get the number of filter headers
 - `getbestblockhash` - Get the best block hash
 - `decoderawtransaction` `tx` - `Decode the given raw hex transaction`
     - `tx` - Transaction encoded in hex to decode

#### Wallet
 - `rescan` `[options]` - Rescan for wallet UTXOs
    - `--force` - Clears existing wallet records. Warning! Use with caution!
    - `--batch-size <value>` - Number of filters that can be matched in one batch
    - `--start <value>` - Start height
    - `--end <value>` - End height
    - `--ignorecreationtime` - Ignores the wallet creation date and will instead do a full rescan
 - `isempty` - Checks if the wallet contains any data
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
 - `opreturncommit` `message` `[options]` - Creates OP_RETURN commitment transaction
    - `message` - message to put into OP_RETURN commitment
    - `--hashMessage` - should the message be hashed before commitment
    - `--feerate <value>` - Fee rate in sats per virtual byte
 - `lockunspent` `unlock` `transactions` - Temporarily lock (unlock=false) or unlock (unlock=true) specified transaction outputs.
    - `unlock` - Whether to unlock (true) or lock (false) the specified transactions
    - `transactions` - The transaction outpoints to unlock/lock


#### Network
 - `getpeers` - List the connected peers
 - `stop` - Request a graceful shutdown of Bitcoin-S
 - `sendrawtransaction` `tx` `Broadcasts the raw transaction`
    - `tx` - Transaction serialized in hex

#### PSBT
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
