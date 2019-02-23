### Syncing for the first time

To start syncing our block headers, we need to indicate a header to start at. Currently our library only supports syncing from the beginning of time, aka the genesis block. We have some logic inside of our storage mechanisms to handle the case of seeding the database with the genesis header, then each new row added must reference the previous row in the database (thus forming a blockchain). 

Here is some example code to start syncing our spv node with all block headers on the network 

```scala
package org.bitcoins.node

import akka.actor.ActorRef
import org.bitcoins.core.protocol.blockchain.TestNetChainParams
import org.bitcoins.node.constant.Constants
import org.bitcoins.node.models.BlockHeaderTable
import org.bitcoins.node.networking.sync.BlockHeaderSyncActor
import slick.driver.PostgresDriver.api._

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
/**
  * Created by chris on 8/29/16.
  */
object Main extends App {
  override def main(args : Array[String]) = {
    //creates the 'block_headers' table, if it exists alread you can remove these 4 lines
    val table = TableQuery[BlockHeaderTable]
    val db = Constants.database
    Await.result(Constants.database.run(table.schema.create),3.seconds)
    db.close()

    //create a BlockHeaderSyncActor
    val blockHeaderSyncActor: ActorRef = BlockHeaderSyncActor(Constants.actorSystem, Constants.dbConfig, Constants.networkParameters)
    val genesisBlockHash = TestNetChainParams.genesisBlock.blockHeader.hash
    //indicates to start the header sync at the genesis hash
    val startHeader = BlockHeaderSyncActor.StartHeaders(Seq(genesisBlockHash))
    //send the block header sync actor a message indicating to start the sync
    blockHeaderSyncActor ! startHeader
  }
}
```

You will start receiving block headers from a node on the peer to peer network, and those headers will be saved in persistent storage, for more information persistent storage read the [database doc](https://github.com/Christewart/bitcoin-s-spv-node/blob/database_documentation/doc/database_setup.md).

### Syncing after being off for awhile

Another scenario users can have is that they have powered down their node for awhile, and want to sync all blockheaders from the network to get the current state of the blockchain. You can do this with this code

```scala
package org.bitcoins.node

import org.bitcoins.node.constant.Constants
import org.bitcoins.node.networking.sync.BlockHeaderSyncActor
object Main extends App {
  override def main(args : Array[String]) = {
    val blockHeaderSyncActor = BlockHeaderSyncActor(Constants.actorSystem, Constants.dbConfig, Constants.networkParameters)
    blockHeaderSyncActor ! BlockHeaderSyncActor.StartAtLastSavedHeader
  }
}
```

Once the sync is complete, your actor will receive a [BlockHeaderSyncActor.SuccessfulSyncReply](https://github.com/bitcoin-s/bitcoin-s-spv-node/blob/master/src/main/scala/org/bitcoins/spvnode/networking/sync/BlockHeaderSyncActor.scala#L244) message indicating the sync was successful. 

