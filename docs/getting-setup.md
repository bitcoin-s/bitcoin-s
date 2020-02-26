---
id: getting-setup
title: Getting Bitcoin-S installed on your machine
---

## Getting Setup With Bitcoin-S

## Step 1: Java and Scala

The first step in getting setup will be getting the [Java Development Kit](https://www.oracle.com/java/technologies/javase-downloads.html) (JDK) installed on your machine. Bitcoin-S works best with Java 8 but _should_ also work with Java 11 and Java 13.

Once java is setup on your machine (try running `javac -version`), you are ready to download and install the [Scala Build Tool](https://www.scala-sbt.org/download.html) (sbt). Note that running `sbt` for the first time will take a while.

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
```

## Step 4 (Optional): Discreet Log Contract Branch

In order to run the Bitcoin-S server with DLCs enabled, you will have to checkout the `dlc` feature branch:

```bashrc
git fetch origin
git checkout dlc
git submodule update
```

and then test that `Secp256k1Context.isEnabled()` as in Step 2.

## Step 5: Setting Up A Bitcoin-S Server (Neutrino Node)

We are finally ready to start running some programs! Follow the [instructions here](applications/server#building-the-server) to build the server. Then, follow [these instructions](applications/cli) to setup the CLI (note that this will require you install some graalvm things as detailed in the instructions).

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
./app/cli/target/graalvm-native-image/bitcoin-s-cli getnewaddress
```

To fund your wallet, you should use the CLI's `getnewaddress` command and then run

```bashrc
$HOME/.bitcoin-s/binaries/bitcoind/bitcoin-0.18.99/bin/bitcoin-cli --datadir=[path/to/conf/directory] --rpcport=18332 generatetoaddress 200 [address]
```

There is currently a bug on regtest where the server is unable to handle too many blocks at once, so when generating more than a couple blocks (like above), it is recommended you shut down your server and restart it after the blocks have been created.

## Step 6 (Optional): Moving To Testnet

To run your Bitcoin-S Server on testnet, simply change `network = testnet3` and change your `peers = ["neutrino.testnet3.suredbits.com:18333"] ` in your `.bitcoin-s/bitcoin-s.conf` file. This will allow you to connect to Suredbits' neutrino-enabled `bitcoind` node. Keep in mind then when you restart your server, it will begin initial sink which will take many hours as all block filters for all testnet blocks will be downloaded. If you wish to speed this process up, download [this snapshot](https://s3-us-west-2.amazonaws.com/www.suredbits.com/testnet-chaindump-2-25-2020.zip), unzip it and put the file in your `$HOME/.bitcoin-s/testnet3` directory and then from there, run

```bashrc
cat chaindump.sql | sqlite3 chaindb.sqlite
```

This should take a couple minutes to execute, but once it is done, you will only have a short while left to sync once you start your server.