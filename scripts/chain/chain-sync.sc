import akka.actor.ActorSystem
import org.bitcoins.chain.api.ChainApi
import org.bitcoins.chain.blockchain._
import org.bitcoins.chain.blockchain.sync._
import org.bitcoins.chain.models._
import org.bitcoins.rpc.client.common._
import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.rpc.config.BitcoindInstance
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.util.AsyncUtil
import scala.concurrent._
import scala.concurrent.duration.DurationInt

implicit val system = ActorSystem(s"chain-sync-script-${System.currentTimeMillis()}")
implicit val ec = system.dispatcher

// We are assuming that a `bitcoind` regtest node is running the background.
// You can see our `bitcoind` guides to see how to connect
// to a local or remote `bitcoind` node.
val bitcoindInstance = BitcoindInstance.fromDatadir()
val rpcCli = BitcoindRpcClient(bitcoindInstance)

// Next, we need to create a way to monitor the chain:
val getBestBlockHash = () => rpcCli.getBestBlockHash

val getBlockHeader = { hash: DoubleSha256DigestBE =>
  rpcCli.getBlockHeader(hash).map(_.blockHeader)
}


// set a data directory
import java.nio.file.Files
val datadir = Files.createTempDirectory("bitcoin-s-test")
println(s"Created new datadir for bitcoin-s=${datadir.toAbsolutePath}")
// set the current network to regtest
import com.typesafe.config.ConfigFactory
val config = ConfigFactory.parseString {
  """
    | bitcoin-s {
    |   network = regtest
    |   logging {
    |     level = INFO
    |   }
    | }
    |""".stripMargin
}

import org.bitcoins.chain.config.ChainAppConfig
implicit val chainConfig = ChainAppConfig(datadir, config)

// Initialize the needed database tables if they don't exist:
val chainProjectInitF = chainConfig.initialize()
val blockHeaderDAO = BlockHeaderDAO()
val compactFilterHeaderDAO = CompactFilterHeaderDAO()
val compactFilterDAO = CompactFilterDAO()

// Now, do the actual syncing:
val chainHandlerF = chainProjectInitF.flatMap { _ =>
  ChainHandler.fromDatabase(blockHeaderDAO, compactFilterHeaderDAO, compactFilterDAO)
}
/** Helper method to check if bitcoin-s and bitcoind are in sync */
def isSynced(chainApi: ChainApi): Future[Boolean] = {
  val bestRpcHashF = getBestBlockHash()
  for {
    bestRpcHash <- bestRpcHashF
    bestChainHandlerHash <- chainApi.getBestBlockHash
  } yield {
    bestRpcHash == bestChainHandlerHash
  }
}

val syncedChainApiF = for {
  _ <- chainProjectInitF
  handler <- chainHandlerF
  synced <- ChainSync.sync(handler, getBlockHeader, getBestBlockHash)
  _ <- AsyncUtil.retryUntilSatisfiedF(() => isSynced(synced))
} yield synced


val syncResultF = syncedChainApiF.flatMap { chainApi =>
  val countF = chainApi.getBlockCount

  countF.map(count => println(s"chain api blockcount=${count}"))

  rpcCli.getBlockCount.map(count => println(s"bitcoind blockcount=${count}"))

  countF
}

syncResultF.onComplete { case result =>
  if (result.isSuccess) {
    println(s"Congrats! Your bitcoin-s chain project synced ${result.get}")
  } else {
    println(s"There was an error syncing your node=${result.failed.get}")
  }

}