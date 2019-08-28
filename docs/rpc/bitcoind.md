---
id: rpc-bitcoind
title: bitcoind/Bitcoin Core
---

> Note: `bitcoin-s-bitcoind-rpc` requires you to have `bitcoind` (Bitcoin Core daemon) installed. Grab this at [bitcoincore.org](https://bitcoincore.org/en/download/)

The Bitcoin Core RPC client in Bitcoin-S currently supports the Bitcoin Core 0.16 and 0.17
version lines. It can be set up to work with both local and remote Bitcoin Core servers.

## Connecting to a local `bitcoind` instance

### Getting started quickly, with default options:

```scala mdoc:compile-only
import scala.concurrent._
import akka.actor.ActorSystem

import org.bitcoins.{rpc, core}
import core.currency.Bitcoins
import rpc.client.common._

implicit val system = ActorSystem.create()
implicit val ec: ExecutionContext = system.dispatcher

// this reads authentication credentials and
// connection details from the default data
// directory on your platform
val client = BitcoindRpcClient.fromDatadir()

val balance: Future[Bitcoins] = for {
  _ <- client.start()
  balance <- client.getBalance
} yield balance
```

## Connecting to a remote `bitcoind`

First, we create a secure connection to our `bitcoind` instance by setting
up a SSH tunnel:

```bash
$ ssh -L 8332:localhost:8332  \
         my-cool-user@my-cool-website.com
```

> Note: the port number '8332' is the default for mainnet. If you want to
> connect to a testnet `bitcoind`, the default port is '18332'

Now that we have a secure connection between our remote `bitcoind`, we're
ready to create the connection with our RPC client

```scala mdoc:compile-only
import akka.actor.ActorSystem
import java.net.URI
import scala.concurrent._

import org.bitcoins.core.config._
import org.bitcoins.rpc.config._
import org.bitcoins.rpc.client.common._

val username = "FILL_ME_IN" //this username comes from 'rpcuser' in your bitcoin.conf file
val password = "FILL_ME_IN" //this password comes from your 'rpcpassword' in your bitcoin.conf file

val authCredentials = BitcoindAuthCredentials.PasswordBased(
  username = username,
  password = password
)

val bitcoindInstance = {
  BitcoindInstance (
    network = MainNet,
    uri = new URI(s"http://localhost:${MainNet.port}"),
    rpcUri = new URI(s"http://localhost:${MainNet.rpcPort}"),
    authCredentials = authCredentials
  )
}

implicit val system: ActorSystem = ActorSystem.create()
implicit val ec: ExecutionContext = system.dispatcher

val rpcCli = new BitcoindRpcClient(bitcoindInstance)

rpcCli.getBalance.onComplete { case balance =>
  println(s"Wallet balance=${balance}")
  system.terminate()
}
```

## Error handling

All errors returned by Bitcoin Core are mapped to a corresponding
[`BitcoindException`](https://github.com/bitcoin-s/bitcoin-s/blob/master/bitcoind-rpc/src/main/scala/org/bitcoins/rpc/BitcoindException.scala).
These exceptions contain an error code and a message. `BitcoindException` is a sealed
trait, which means you can easily pattern match exhaustively. Of course, other errors
could also happen: network errors, stack overflows or out-of-memory errors. The provided
class is only intended to cover errors returned by Bitcoin Core. An example of how error
handling could look:

```scala mdoc:compile-only
import org.bitcoins.rpc.client.common._
import org.bitcoins.rpc.BitcoindWalletException
import org.bitcoins.core.crypto._
import org.bitcoins.core.protocol._
import org.bitcoins.core.currency._

import scala.concurrent._

implicit val system = akka.actor.ActorSystem()
implicit val ec = system.dispatcher

// let's assume you have an already running client,
// so there's no need to start this one
val cli = BitcoindRpcClient.fromDatadir()

// let's also assume you have a bitcoin address
val address: BitcoinAddress = ???

val txid: Future[DoubleSha256DigestBE] =
  cli.sendToAddress(address, 3.bitcoins).recoverWith {
    case BitcoindWalletException.UnlockNeeded(_) =>
      cli.walletPassphrase("my_passphrase", 60).flatMap { _ =>
        cli.sendToAddress(address, 3.bitcoins)
      }
  }
```
