---
id: build-oracle-server
title: Building the Oracle Server
---

The Oracle Server is a DLC Oracle with functionality for creating events and attesting to them.
The Oracle Server is interactable through the RPC interface.

The following a guide is for how to build the oracle server.
If you are looking for the documentation on how to use the oracle server,
checkout [this page](oracle-server.md).

## Step 1: Java and Scala

To get started you will need Java, Scala, and some other nice tools installed, luckily the Scala team has an easy setup process!

Simply follow the instructions in [this short blog](https://www.scala-lang.org/2020/06/29/one-click-install.html) to get started.

## Step 2: Bitcoin-S Repository

Now, it is time to clone the [Bitcoin-S repository](https://github.com/bitcoin-s/bitcoin-s/) by running

```bashrc
git clone --recursive git@github.com:bitcoin-s/bitcoin-s.git
```

or alternatively, if you do not have ssh setup with github, you can run

```bashrc
git clone --recursive https://github.com/bitcoin-s/bitcoin-s.git
```

Next, you will want to execute the commands

```bashrc
cd bitcoin-s
git submodule update
```

to download the secp256k1 submodule, this is so cryptographic functions like signing will be faster.

## Step 3: Building the Oracle Server

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

Alternatively you can run the server by just using:

```bash
sbt oracleServer/run
```

## Step 4: Configuration

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

For more information on configuring the server please see our [configuration](../config/configuration.md) document.

For more information on how to use our built in `cli` to interact with the server please see the [cli docs](../applications/cli.md).
