# Getting Setup With Bitcoin-S

## Step 1: Java and Scala

The first step in getting setup will be getting the [Java Development Kit](https://www.oracle.com/java/technologies/javase-downloads.html) (JDK) installed on your machine. Bitcoin-S works best with Java 8 but _should_ also work with Java 11.

Once java is setup on your machine (try running `javac -version`), you are ready to download and install the [Scala Build Tool](https://www.scala-sbt.org/download.html) (sbt). Note that running `sbt` for the first time will take a while.

## Step 2: Bitcoin-S Repository

Now, it is time to clone the [Bitcoin-S repository](https://github.com/bitcoin-s/bitcoin-s/) by running

```bashrc
git clone git@github.com:bitcoin-s/bitcoin-s.git
```

or alternatively, if you do not have ssh setup with github, you can run

```bashrc
git clone https://github.com/bitcoin-s/bitcoin-s.git
```

Next, you will want to execute the commands

```bashrc
cd bitcoin-s
git submodule update
```

to download the secp256k1 submodule.

You should be able to test your secp256k1 installation by running `sbt console` in your bitcoin-s directory and then running

```scala
scala> import org.bitcoin._
import org.bitcoin._

scala> Secp256k1Context.isEnabled()
res0: Boolean = true
```

Lastly, you can test that your bitcoin-s build is functional by running

```bashrc
sbt test
```

## Step 3: Configuration

Now that we have the bitcoin-s repo setup, we want to create our application configurations. This is done by creating a `bitcoin-s.conf` file at `$HOME/.bitcoin-s`. [Here is an example configuration file](https://bitcoin-s.org/docs/next/applications/configuration#example-configuration-file). The only thing that you will _need_ to change is the `peers` list to which you will want to add `localhost:18444` if you want to run in regtest.

Once the bitcoin-s configuration is all done, I recommend creating a directory someplace in which to run your `bitcoind` node. Once you have this directory created, add the following `bitcoin.conf` file to it

```
regtest=1
server=1
rpcuser=[your username here]
rpcpassword=[your password here]
rpcport=18332
zmqpubrawblock=tcp://127.0.0.1:29000
zmppubrawtx=tcp://127.0.0.1:29000
addresstype=p2sh-segwit
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

We are finally ready to start running some programs! Follow the [instructions here](https://bitcoin-s.org/docs/next/applications/server#building-the-server) to build the server. Then, follow [these instructions](https://bitcoin-s.org/docs/next/applications/cli) to setup the CLI (note that this will require you install some graalvm things as detailed in the instructions).

Now, you want to run your `bitcoind` in regtest by doing the following command:

```bashrc
./binaries/bitcoind/bitcoin-0.18.99/bin/bitcoind --datadir=[path/to/conf/directory] --rpcport=18332
```

Once the node is running, you should be able to start your bitcoin-s server with

```bashrc
./app/server/target/universal/stage/bin/bitcoin-s-server
```

and once this is done, you should be able to communicate with the server using

```bashrc
./app/cli/target/graalvm-native-image/bitcoin-s-cli getblockcount
```

To fund your wallet, you should use the CLI's `getnewaddress` command and then run

```bashrc
./binaries/bitcoind/bitcoin-0.18.99/bin/bitcoin-cli --datadir=[path/to/conf/directory] --rpcport=18332 generatetoaddress 200 [address]
```

There is currently a bug on regtest where the server is unable to handle too many blocks at once, so when generating more than a couple blocks (like above), it is recommended you shut down your server and restart it after the blocks have been created.