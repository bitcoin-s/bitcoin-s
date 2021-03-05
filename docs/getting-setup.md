---
id: getting-setup
title: Getting Bitcoin-S installed on your machine
---

> This documentation is intended for setting up development of bitcoin-s.
> If you want to just install bitcoin-s rather than develop, 
> see [getting-started](getting-started.md)

## Getting Setup With Bitcoin-S

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
<!-- END doctoc -->

- [Step 1: Java and Scala](#step-1-java-and-scala)
- [Step 2: Bitcoin-S Repository](#step-2-bitcoin-s-repository)
- [Step 3: Configuration](#step-3-configuration)
- [Step 4 (Optional): Discreet Log Contract Branch](#step-4-optional-discreet-log-contract-branch)
- [Step 5: Setting Up A Bitcoin-S Server (Neutrino Node)](#step-5-setting-up-a-bitcoin-s-server)
- [Step 6 (Optional): Moving To Testnet](#step-6-optional-moving-to-testnet)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->


## Step 1: Java and Scala

To get started you will need Java, Scala, and some other nice tools installed, luckily the Scala team has an easy setup process!

Simply follow the instructions in [this short blog](https://www.scala-lang.org/2020/06/29/one-click-install.html) to get started.

## Step 2: Bitcoin-S Repository

Now, it is time to clone the [Bitcoin-S repository](https://github.com/bitcoin-s/bitcoin-s/) by running

```bashrc
git clone --depth 100 --recursive git@github.com:bitcoin-s/bitcoin-s.git
```

or alternatively, if you do not have ssh setup with github, you can run

```bashrc
git clone --depth 100 --recursive https://github.com/bitcoin-s/bitcoin-s.git
```

Next, you will want to execute the commands

```bashrc
cd bitcoin-s
git submodule update
```

to download the secp256k1 submodule.

You should be able to test your secp256k1 installation by running `sbt core/console` in your bitcoin-s directory and then running

`sbt coreTest/test`

### Optional

#### Verify libsecp256k1 installation 

To verify you are actually using libsecp256k1 rather than our bouncy castle crypto implementation you can do the following

`sbt secp256k1jni/console`

and then type in 
```scala
scala> import org.bitcoin._
import org.bitcoin._

scala> Secp256k1Context.isEnabled()
SLF4J: Failed to load class "org.slf4j.impl.StaticLoggerBinder".
SLF4J: Defaulting to no-operation (NOP) logger implementation
SLF4J: See http://www.slf4j.org/codes.html#StaticLoggerBinder for further details.
res0: Boolean = true
```

where the important thing is that the function returns `true`, and you can ignore SLF4J errors.

Note: To exit the `sbt console`, you can execute `:quit`, and for general help, run `:help`.

#### Running full test suite

> WARNING: This should not be done on low resource machines. Running the entire test suite requires at minimum of 4GB
> of RAM on the machine you are running this on.

To run the entire test suite, you need to download all bitcoind instances and eclair instances. This is needed for unit tests
or binding bitcoin-s to a bitcoind instance if you do not have locally running instances. 

```bashrc
sbt downloadBitcoind
sbt downloadEclair
```

If you want to run the entire test suite you can run the following command after you download bitcoind
and eclair. 

```bashrc
sbt test
```

## Step 3: Configuration

Now that we have the bitcoin-s repo setup, we want to create our application configurations. This is done by creating a `bitcoin-s.conf` file at `$HOME/.bitcoin-s`. [Here is an example configuration file](config/configuration.md#example-configuration-file). The only thing that you will _need_ to change is the `peers` list to which you will want to add `"localhost:18444"` if you want to run in regtest.

Once the bitcoin-s configuration is all done, I recommend creating a directory someplace in which to run your `bitcoind` node. Once you have this directory created, add the following `bitcoin.conf` file to it

```
regtest=1
server=1
rpcuser=[your username here]
rpcpassword=[your password here]
daemon=1
blockfilterindex=1
peerblockfilters=1
debug=1
txindex=1
zmqpubrawblock=tcp://127.0.0.1:29000
zmqpubrawtx=tcp://127.0.0.1:29000
```

## Step 4 (Optional): Discreet Log Contract Branch

In order to run the Bitcoin-S server with DLCs enabled, you will have to checkout the `adaptor-dlc` feature branch:

```bashrc
git fetch origin
git checkout adaptor-dlc
git submodule update
```

and then finally test that `Secp256k1Context.isEnabled()` as in Step 2.

If you're looking to set up a DLC Oracle instead go to the [oracle server docs](oracle/oracle-server.md).

## Step 5: Setting Up A Bitcoin-S Server

We are finally ready to start running some programs! Follow the [instructions here](applications/server.md#building-the-server) to build the server. Then, follow [these instructions](applications/cli.md) to setup the CLI.

There are 2 ways to use the bitcoin-s server. It can either be as a neutrino node or use bitcoind as a backend.
This can be configured by the configuration option `bitcoin-s.node.mode` choosing either `neutrino` or `bitcoind`.

### Option A: Neutrino Server

To use a neutrino server you need to be paired with a bitcoin node that can serve compact filters.
[Suredbits](https://suredbits.com/) runs a mainnet and testnet node you can connect to them by setting your `peers` config option to:

Mainnet:

`bitcoin-s.node.peers = ["neutrino.suredbits.com:8333"]`

Testnet:

`bitcoin-s.node.peers = ["neutrino.testnet3.suredbits.com:18333"]`

If you would like to use your own node you can either use the bitcoind backend option or connect to your own compatible node.
There is no released version of bitcoind that is neutrino compatible, so you will either have to compile the latest `master` yourself, or use the experimental version provided by running `sbt downloadBitcoind`. 

After building your bitcoin-s server, properly configuring it to be in `neutrino` mode you can start your server with:

```bashrc
./app/server/target/universal/stage/bin/bitcoin-s-server
```

and once this is done, you should be able to communicate with the server using

```bashrc
./app/cli/target/universal/stage/bitcoin-s-cli getnewaddress
```

### Option B: Bitcoind Backend

If you already have a bitcoind node running and would like to connect your bitcoin-s server to it you can set your node's mode to `bitcoind`.

You will need to configure bitcoin-s to be able to find your bitcoind.
If you would only like bitcoin-s to connect to bitcoind and start it itself then you only need to properly set the `rpcuser`, and `rpcpassword` options.
If you would like bitcoin-s to launch bitcoind on start up you will need to set the other configuration options.
These options should default to use the latest bitcoind downloaded from `sbt downloadBitcoind`.

```$xslt
bitcoin-s {
    bitcoind-rpc {
        # bitcoind rpc username
        rpcuser = user
        # bitcoind rpc password
        rpcpassword = password

        # Binary location of bitcoind
        binary = ${HOME}/.bitcoin-s/binaries/bitcoind/bitcoin-0.20.1/bin/bitcoind
        # bitcoind datadir
        datadir = ${HOME}/.bitcoin
        # bitcoind network binding
        bind = localhost
        # bitcoind p2p port
        port = 8333
        # bitcoind rpc binding
        rpcbind = localhost
        # bitcoind rpc port
        rpcport = 8332
        # bitcoind zmq port for all services
        zmqport = 29000
    }
```

## Step 6 (Optional): Moving To Testnet

To run your Bitcoin-S Server on testnet, simply change `network = testnet3` and change
your `peers = ["neutrino.testnet3.suredbits.com:18333"] ` in your `.bitcoin-s/bitcoin-s.conf` file.
This will allow you to connect to Suredbits' neutrino-enabled `bitcoind` node.
Keep in mind then when you restart your server, it will begin initial sync which will take
many hours as all block filters for all testnet blocks will be downloaded.
If you wish to speed this process up,
download [this snapshot](https://s3-us-west-2.amazonaws.com/www.suredbits.com/chaindb-testnet-2021-02-03.zip), unzip it and put the file in your `$HOME/.bitcoin-s/testnet3` directory and then from there, run

```bashrc
$ unzip chaindb-testnet-2021-02-03.zip
$ mv chaindb.sqlite ~/.bitcoin-s/testnet/
```

This should take a couple minutes to execute, but once it is done, you will only have a short while left to sync once you start your server.