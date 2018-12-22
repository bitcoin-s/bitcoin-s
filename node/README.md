[![Build Status](https://travis-ci.org/bitcoin-s/bitcoin-s-spv-node.svg?branch=master)](https://travis-ci.org/bitcoin-s/bitcoin-s-spv-node) [![Coverage Status](https://coveralls.io/repos/github/bitcoin-s/bitcoin-s-spv-node/badge.svg?branch=master)](https://coveralls.io/github/bitcoin-s/bitcoin-s-spv-node?branch=master)

This is an implementation of an SPV node on the Bitcoin network using Scala & [Bitcoin-S-Core](https://github.com/bitcoin-s/bitcoin-s-core). 

Our implementation relies heavily on [Akka](http://akka.io/), which has an implementation of the [Actor model](https://en.wikipedia.org/wiki/Actor_model) in Scala. If you want to read more about Akka and what it is/how it is used, it is best to start reading [here](http://doc.akka.io/docs/akka/2.4/scala.html).
# Examples

Look inside of [Main.scala](https://github.com/Christewart/bitcoin-s-spv-node/blob/networking/src/main/scala/org/bitcoins/spvnode/Main.scala) for example of creating a [`PaymentActor`](https://github.com/Christewart/bitcoin-s-spv-node/blob/networking/src/main/scala/org/bitcoins/spvnode/networking/PaymentActor.scala), that montiors an address. Once a transaction that pays to the address is included in a block, it sends a message back to your actor saying a payment was successful. 

```scala
package org.bitcoins.node

import org.bitcoins.core.config.TestNet3
import org.bitcoins.core.crypto.{DoubleSha256Digest, Sha256Hash160Digest}
import org.bitcoins.core.protocol.blockchain.TestNetChainParams
import org.bitcoins.core.protocol.{BitcoinAddress, P2PKHAddress}
import org.bitcoins.core.util.BitcoinSUtil
import org.bitcoins.node.constant.Constants
import org.bitcoins.node.networking.PaymentActor
import org.bitcoins.node.networking.sync.BlockHeaderSyncActor

/**
  * Created by chris on 8/29/16.
  */
object Main extends App {


  override def main(args : Array[String]) = {
    val blockHeaderSyncActor = BlockHeaderSyncActor(Constants.actorSystem)
    val gensisBlockHash = TestNetChainParams.genesisBlock.blockHeader.hash
    val startHeader = BlockHeaderSyncActor.StartHeaders(Seq(gensisBlockHash))
    blockHeaderSyncActor ! startHeader
  }
}
```

If you want to see more logging for the networking stuff, adjust your [logback.xml](https://github.com/Christewart/bitcoin-s-spv-node/blob/networking/src/main/resources/logback.xml#L18) file to DEBUG.

After that, you are ready to fire up your spv node with this command:

```bash
chris@chris-870Z5E-880Z5E-680Z5E:~/dev/bitcoins-spv-node$ sbt run
```

After that, you should start seeing headers being synced to your node. The headers are stored inside of a file called `block_headers.dat` file inside of `src/main/resources`. Note that this does not use any checkpointing system, so to sync up all ~930,000 headers on TestNet3 will take awhile. 

### Perisistent storage

For more information on how we store block headers in our spv node, please read [database_documentation](https://github.com/Christewart/bitcoin-s-spv-node/blob/database_documentation/doc/database_setup.md)

### Syncing our node 

For more information on syncing our node with the peer to peer network, please read [header_sync](https://github.com/Christewart/bitcoin-s-spv-node/blob/database_documentation/doc/header_sync.md)
