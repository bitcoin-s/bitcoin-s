---
id: node
title: Light Client
---

Bitcoin-s has node module that allows you to connect to the p2p network.

### Neutrino Node

Bitcoin-s has experimental support for neutrino which is a new lite client proposal on the bitcoin p2p network. You can
read more about how neutrino works [here](https://suredbits.com/neutrino-what-is-it-and-why-we-need-it/). At this time, 
bitcoin-s only supports connecting to one trusted peer.

#### Limitations

Currently, the node does not have an active mempool.
It is only aware of transactions it broadcasts and ones confirmed in blocks.

#### Callbacks

Bitcoin-S support call backs for the following events that happen on the bitcoin p2p network:

1. onTxReceived
2. onBlockReceived
3. onMerkleBlockReceived
4. onCompactFilterReceived

That means every time one of these events happens on the p2p network, we will call your callback
so that you can be notified of the event. These callbacks will be run after the message has been
recieved and will execute sequentially. If any of them fail an error log will be output and the remainder of the callbacks will continue.
Let's make an easy one

#### Example

Here is an example of constructing a neutrino node and registering a callback so you can be notified of an event.

To run the example, we need a bitcoind binary that has neutrino support.
Bitcoin Core only has p2p neutrino support as of version 0.21.0.
You will need to use a version of Bitcoin Core at least as old as 0.21.0.
For your node to be able to service these filters you will need set
`blockfilterindex=1` and `peerblockfilters=1` in your `bitcoin.conf` file.

```scala mdoc:invisible
import akka.actor.ActorSystem
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.node._
import org.bitcoins.rpc.client.common.BitcoindVersion
import org.bitcoins.testkit.node._
import org.bitcoins.testkit.node.fixture._
import org.bitcoins.testkit.rpc._
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit._
import org.bitcoins.testkit.chain._
import scala.concurrent._
import scala.concurrent.duration._
import java.nio.file.Files
import com.typesafe.config.ConfigFactory
```

```scala mdoc:compile-only

implicit val system = ActorSystem(s"node-example")
implicit val ec = system.dispatcher

//we also require a bitcoind instance to connect to
//so let's start one (make sure you ran 'sbt downloadBitcoind')
val instance = BitcoindRpcTestUtil.instance(versionOpt = Some(BitcoindVersion.Experimental))
val p2pPort = instance.p2pPort
val bitcoindF = BitcoindRpcTestUtil.startedBitcoindRpcClient(instance)

//contains information on how to connect to bitcoin's p2p info
val peerF = bitcoindF.map(b => NodeUnitTest.createPeer(b))

// set a data directory
val prefix = s"node-example-${System.currentTimeMillis()}"
val datadir = Files.createTempDirectory(prefix)

val tmpDir = BitcoinSTestAppConfig.tmpDir()
// set the current network to regtest
val config = ConfigFactory.parseString {
    s"""
    | bitcoin-s {
    |   network = regtest
    |   node {
    |        mode = neutrino # neutrino, spv
    |
    |        peers = ["127.0.0.1:$p2pPort"] # a list of peer addresses in form "hostname:portnumber"
    |        # (e.g. "neutrino.testnet3.suredbits.com:18333")
    |        # Port number is optional, the default value is 8333 for mainnet,
    |        # 18333 for testnet and 18444 for regtest.
    |   }
    | }
    |""".stripMargin
}

implicit val appConfig = BitcoinSAppConfig(datadir, config)
implicit val chainConfig = appConfig.chainConf
implicit val nodeConfig = appConfig.nodeConf

val initNodeF = nodeConfig.start()

//the node requires a chainHandler to store block information
//use a helper method in our testkit to create the chain project
val chainApiF = for {
  chainHandler <- ChainUnitTest.createChainHandler()
} yield chainHandler


//yay! All setup done, let's create a node and then start it!
val nodeF = for {
  _ <- chainApiF
  peer <- peerF
} yield {
    NeutrinoNode(nodePeer = peer,
               nodeConfig = nodeConfig,
               chainConfig = chainConfig,
               actorSystem = system,
               initialSyncDone = None)
}

//let's start it
val startedNodeF = nodeF.flatMap(_.start())

//let's make a simple callback that print's the
//blockhash everytime we receive a block on the network
val blockReceivedFunc: OnBlockReceived = { block: Block =>
Future.successful(
  println(s"Received blockhash=${block.blockHeader.hashBE}"))
}

// Create callback
val nodeCallbacks = NodeCallbacks.onBlockReceived(blockReceivedFunc)

// Add call to our node's config
nodeConfig.addCallbacks(nodeCallbacks)

//let's test it out by generating a block with bitcoind!

val genBlockF = for {
  bitcoind <- bitcoindF
  addr <- bitcoind.getNewAddress
  hashes <- bitcoind.generateToAddress(1,addr)
} yield ()

//you should see our callback print a block hash
//when running this code

//cleanup
val cleanupF = for {
  _ <- genBlockF
  bitcoind <- bitcoindF
  node <- startedNodeF
  x = NeutrinoNodeConnectedWithBitcoind(node.asInstanceOf[NeutrinoNode],bitcoind)
  _ <- NodeUnitTest.destroyNodeConnectedWithBitcoind(x)
} yield ()

Await.result(cleanupF, 60.seconds)
```