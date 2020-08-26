---
id: getting-setup
title: Getting Bitcoin-S installed on your machine
---

## Getting Setup With Bitcoin-S

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
<!-- END doctoc -->

- [Step 1: Java and Scala](#step-1-java-and-scala)
- [Step 2: Bitcoin-S Repository](#step-2-bitcoin-s-repository)
- [Step 3: Configuration](#step-3-configuration)
- [Step 4 (Optional): Discreet Log Contract Branch](#step-4-optional-discreet-log-contract-branch)
- [Step 5: Setting Up A Bitcoin-S Server (Neutrino Node)](#step-5-setting-up-a-bitcoin-s-server-neutrino-node)
- [Step 6 (Optional): Moving To Testnet](#step-6-optional-moving-to-testnet)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

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

to download the secp256k1 submodule.

You should be able to test your secp256k1 installation by running `sbt core/console` in your bitcoin-s directory and then running

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

We will now download all of the bitcoind and eclair binaries needed with the following two commands

```bashrc
sbt downloadBitcoind
sbt downloadEclair
```

Lastly, you can test that your bitcoin-s build is functional by running

```bashrc
sbt test
```

## Step 3: Configuration

Now that we have the bitcoin-s repo setup, we want to create our application configurations. This is done by creating a `bitcoin-s.conf` file at `$HOME/.bitcoin-s`. [Here is an example configuration file](applications/configuration#example-configuration-file). The only thing that you will _need_ to change is the `peers` list to which you will want to add `"localhost:18444"` if you want to run in regtest.

Once the bitcoin-s configuration is all done, I recommend creating a directory someplace in which to run your `bitcoind` node. Once you have this directory created, add the following `bitcoin.conf` file to it

```
regtest=1
server=1
rpcuser=[your username here]
rpcpassword=[your password here]
rpcport=18332
daemon=1
deprecatedrpc=signrawtransaction
blockfilterindex=1
debug=1
txindex=1
zmqpubrawblock=tcp://127.0.0.1:29000
zmqpubrawtx=tcp://127.0.0.1:29000
```

## Step 4 (Optional): Discreet Log Contract Branch

In order to run the Bitcoin-S server with DLCs enabled, you will have to checkout the `dlc` feature branch:

```bashrc
git fetch origin
git checkout adaptor-dlc
git submodule update
```

and then finally test that `Secp256k1Context.isEnabled()` as in Step 2.

## Step 5: Setting Up A Bitcoin-S Server (Neutrino Node)

We are finally ready to start running some programs! Follow the [instructions here](applications/server#building-the-server) to build the server. Then, follow [these instructions](applications/cli) to setup the CLI.

If you wish to use the GUI instead of the CLI, the setup process is the same as building the server but for the project `gui`. That is, you must run

```bashrc
sbt gui/universal:stage
```

to build and then the following (once setup is complete) to run:

```bashrc
./app/gui/target/universal/stage/bin/bitcoin-s-gui
```

Now, you want to run your `bitcoind` in regtest by doing the following command:

```bashrc
$HOME/.bitcoin-s/binaries/bitcoind/bitcoin-0.18.99/bin/bitcoind --datadir=[path/to/conf/directory] --rpcport=18332
```

Once the node is running, you should be able to start your bitcoin-s server with

```bashrc
./app/server/target/universal/stage/bin/bitcoin-s-server
```

and once this is done, you should be able to communicate with the server using

```bashrc
./app/cli/target/universal/stage/bitcoin-s-cli getnewaddress
```

To fund your wallet, you should use the CLI's `getnewaddress` command and then run

```bashrc
$HOME/.bitcoin-s/binaries/bitcoind/bitcoin-0.18.99/bin/bitcoin-cli --datadir=[path/to/conf/directory] --rpcport=18332 generatetoaddress 200 [address]
```

There is currently a bug on regtest where the server is unable to handle too many blocks at once, so when generating more than a couple blocks (like above), it is recommended you shut down your server and restart it after the blocks have been created.

## Step 6 (Optional): Moving To Testnet

To run your Bitcoin-S Server on testnet, simply change `network = testnet3` and change your `peers = ["neutrino.testnet3.suredbits.com:18333"] ` in your `.bitcoin-s/bitcoin-s.conf` file. This will allow you to connect to Suredbits' neutrino-enabled `bitcoind` node. Keep in mind then when you restart your server, it will begin initial sync which will take many hours as all block filters for all testnet blocks will be downloaded. If you wish to speed this process up, download [this snapshot](https://s3-us-west-2.amazonaws.com/www.suredbits.com/testnet-chaindump-2-25-2020.zip), unzip it and put the file in your `$HOME/.bitcoin-s/testnet3` directory and then from there, run

```bashrc
cat chaindump.sql | sqlite3 chaindb.sqlite
```

This should take a couple minutes to execute, but once it is done, you will only have a short while left to sync once you start your server.