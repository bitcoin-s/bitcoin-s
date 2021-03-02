---
id: testkit
title: Testkit
---

## Philosphy of Testkit

The high level of of the bitcoin-s testkit is to mimic and provide functionality to test 3rd party applications.

There are other examples of these in the Scala ecosystem like the `akka-testkit` and `slick-testkit`.

We use this testkit to test bitcoin-s it self.

```scala mdoc:invisible
import akka.actor.ActorSystem
import org.bitcoins.node._
import org.bitcoins.core.protocol.ln.currency._
import org.bitcoins.rpc.client.common._
import org.bitcoins.testkit.node._
import org.bitcoins.testkit.node.fixture._
import org.bitcoins.testkit.rpc._
import org.bitcoins.testkit.eclair.rpc._

import scala.concurrent._
import scala.concurrent.duration._
```

### Testkit for bitcoind

This gives the ability to create and destroy `bitcoind` on the underlying operating system to test against.

Make sure you have run `sbt downloadBitcoind` before running this example, as you need access to the bitcoind binaries.

Our [BitcoindRpcClient](/api/org/bitcoins/rpc/client/common/BitcoindRpcClient) is tested with the functionality provided in the testkit.
A quick example of a useful utility method is [BitcoindRpcTestUtil.startedBitcoindRpcClient()](/api/org/bitcoins/testkit/rpc/BitcoindRpcTestUtil).
This spins up a bitcoind regtest instance on machine and generates 101 blocks on that node.

This gives you the ability to start spending money immediately with that bitcoind node.

```scala mdoc:compile-only

implicit val system = ActorSystem("bitcoind-testkit-example")
implicit val ec = system.dispatcher

//pick our bitcoind version we want to spin up
//you can pick older versions if you want
//we support versions 16-19
val bitcoindV = BitcoindVersion.V19

//create an instance
val instance = BitcoindRpcTestUtil.instance(versionOpt = Some(bitcoindV))

//now let's create an rpc client off of that instance
val bitcoindRpcClientF = BitcoindRpcTestUtil.startedBitcoindRpcClient(instance)

//yay! it's started. Now you can run tests against this.
//let's just grab the block count for an example
val blockCountF = for {
  bitcoind <- bitcoindRpcClientF
  count <- bitcoind.getBlockCount
} yield {
  //run a test against the block count
  assert(count > 0, s"Block count was not more than zero!")
}

//when you are done, don't forget to destroy it! Otherwise it will keep running on the underlying os
val stoppedF = for {
  rpc <- bitcoindRpcClientF
  _ <- blockCountF
  stopped <- BitcoindRpcTestUtil.stopServers(Vector(rpc))
} yield stopped

```

For more information on how the bitcoind rpc client works, see our [bitcoind rpc docs](../rpc/bitcoind.md)

### Testkit for eclair

We have similar utility methods for eclair. Eclair's testkit requires a bitcoind running (which we can spin up thanks to our bitcoind testkit).

Here is an example of spinning up an eclair lightning node, that is connected to a bitcoind and testing your lightning application.

Make sure to run `sbt downloadBitcoind downloadEclair` before running this so you have access to the underlying eclair binares

```scala mdoc:compile-only


//Steps:
//1. Open and confirm channel on the underlying blockchain (regtest)
//2. pay an invoice
//3. Await until the payment is processed
//4. assert the node has received the payment
//5. cleanup

implicit val system = ActorSystem("eclair-testkit-example")
implicit val ec = system.dispatcher

//we need a bitcoind to connect eclair nodes to
lazy val bitcoindRpcClientF: Future[BitcoindRpcClient] = {
    for {
      cli <- EclairRpcTestUtil.startedBitcoindRpcClient()
      // make sure we have enough money to open channels
      address <- cli.getNewAddress
      _ <- cli.generateToAddress(200, address)
    } yield cli
}

//let's create two eclair nodes now
val clientF = for {
  bitcoind <- bitcoindRpcClientF
  e <- EclairRpcTestUtil.randomEclairClient(Some(bitcoind))
} yield e

val otherClientF = for {
  bitcoind <- bitcoindRpcClientF
  e <- EclairRpcTestUtil.randomEclairClient(Some(bitcoind))
} yield e

//great, setup done! Let's run the test
//to verify we can send a payment over the channel
for {
  client <- clientF
  otherClient <- otherClientF
  _ <- EclairRpcTestUtil.openAndConfirmChannel(clientF, otherClientF)
  invoice <- otherClient.createInvoice("abc", 50.msats)
  info <- otherClient.getInfo
  _ = assert(info.nodeId == invoice.nodeId)
  infos <- client.getSentInfo(invoice.lnTags.paymentHash.hash)
  _ = assert(infos.isEmpty)
  paymentId <- client.payInvoice(invoice)
  _ <- EclairRpcTestUtil.awaitUntilPaymentSucceeded(client, paymentId)
  sentInfo <- client.getSentInfo(invoice.lnTags.paymentHash.hash)
} yield {
  assert(sentInfo.head.amount == 50.msats)
}

//don't forget to shutdown everything!
val stop1F = clientF.map(c => EclairRpcTestUtil.shutdown(c))

val stop2F = otherClientF.map(o => EclairRpcTestUtil.shutdown(o))

val stoppedBitcoindF = for {
  bitcoind <- bitcoindRpcClientF
  _ <- BitcoindRpcTestUtil.stopServers(Vector(bitcoind))
} yield ()


val resultF = for {
  _ <- stop1F
  _ <- stop2F
  _ <- stoppedBitcoindF
  _ <- system.terminate()
} yield ()

Await.result(resultF, 180.seconds)
```

### Testkit for core

The testkit functionality for our core module primary consists of generators for property based tests.

A generator is a piece of code that generates a random object for a data strucutre -- such as a `Transaction`.

There is also a robust set of generators available in the [org.bitcoins.testkit.gen](../../testkit-core/src/main/scala/org/bitcoins/testkitcore/gen) package.
This allows you to integrate property based testing into your library and feel confident about implementing your application specific logic correctly.

You can see examples of us using these generators inside of testkit in our [Private Key test cases](https://github.com/bitcoin-s/bitcoin-s/blob/master/crypto-test/src/test/scala/org/bitcoins/crypto/ECPrivateKeyTest.scala)

### Other modules

You may find useful testkit functionality for other modules here

1. [Chain](/api/org/bitcoins/testkit/chain/ChainUnitTest)
2. [Key Manager](/api/org/bitcoins/testkit/keymanager/KeyManagerApiUnitTest)
3. [Wallet](/api/org/bitcoins/testkit/wallet/BitcoinSWalletTest)
4. [Node](/api/org/bitcoins/testkit/node/NodeUnitTest)

In general, you will find constructors and destructors of fixtures that can be useful when testing your applications
if yo uare using any of those modules.