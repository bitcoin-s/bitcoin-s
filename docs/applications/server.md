---
id: server
title: Application Server
---


### App server

The [server](../../app/server) project is the aggregation of these three sub projects

1. [Wallet](../wallet/wallet.md)
2. [Chain](../chain/chain.md)
3. [Node](../node/node.md)

The server project provides a away to access information from these three projects via a JSON RPC.

### Building the server

You can build the server with the [sbt native packager](https://github.com/sbt/sbt-native-packager).
The native packager offers [numerous ways to package the project](https://github.com/sbt/sbt-native-packager#examples).

In this example we are going to use `stage` which will produce bash scripts we can easily execute. You can stage the server with the following command

```bash
 $ sbt appServer/universal:stage
```

This will produce a script to execute bitcoin-s which you can start with

```bash
$ ./app/server/target/universal/stage/bin/bitcoin-s-server
```

If you would like to pass in a custom datadir for your server, you can do

```bash
./app/server/target/universal/stage/bin/bitcoin-s-server --datadir /path/to/datadir/
```

You can also pass in a custom `rpcport` to bind to

```bash
./app/server/target/universal/stage/bin/bitcoin-s-server --rpcport 12345
```

For more information on configuring the server please see our [configuration](../config/configuration.md) document

For more information on how to use our built in `cli` to interact with the server please see [cli.md](cli.md)