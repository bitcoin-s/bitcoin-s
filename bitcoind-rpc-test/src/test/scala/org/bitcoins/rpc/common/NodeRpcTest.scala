package org.bitcoins.rpc.common

import org.bitcoins.core.number.UInt32
import org.bitcoins.rpc.BitcoindException.MiscError
import org.bitcoins.testkit.rpc.BitcoindFixturesFundedCachedNewest

import scala.concurrent.duration._

class NodeRpcTest extends BitcoindFixturesFundedCachedNewest {

  behavior of "NodeRpc"

  it should "be able to abort a rescan of the blockchain" in { case client =>
    // generate some extra blocks so rescan isn't too quick
    client.getNewAddress
      .flatMap(client.generateToAddress(500, _))
      .flatMap { _ =>
        println(s"here")
        val rescanFailedF =
          recoverToSucceededIf[MiscError](client.rescanBlockChain())
        system.scheduler.scheduleOnce(100.millis) {
          println(s"Running scheduled job")
          client
            .abortRescan()
            .failed
            .foreach(err => println(s"err=${err.getMessage}"))
          ()
        }
        rescanFailedF
      }
  }

  it should "be able to ping" in { case client =>
    for {
      _ <- client.ping()
    } yield succeed
  }

  it should "be able to get and set the logging configuration" in {
    case client =>
      for {
        info <- client.logging
        infoNoQt <- client.logging(exclude = Vector("qt"))
      } yield {
        info.keySet.foreach(category => assert(info(category)))
        assert(!infoNoQt("qt"))
      }
  }

  it should "be able to get the memory info" in { case client =>
    for {
      info <- client.getMemoryInfo
    } yield {
      assert(info.locked.used > 0)
      assert(info.locked.free > 0)
      assert(info.locked.total > 0)
      assert(info.locked.locked > 0)
      assert(info.locked.chunks_used > 0)
    }
  }

  it should "be able to get the client's uptime" in { case client =>
    for {
      time <- client.uptime
    } yield assert(time > UInt32(0))
  }

  it should "be able to get help from bitcoind" in { case client =>
    for {
      genHelp <- client.help()
      helpHelp <- client.help("help")
    } yield {
      assert(!genHelp.isEmpty)
      assert(genHelp != helpHelp)
      assert(!helpHelp.isEmpty)
    }
  }

  it should "be able to get network info" in { freshClient =>
    for {
      info <- freshClient.getNetworkInfo
    } yield {
      assert(info.networkactive)
      assert(info.localrelay)
    }
  }

  it should "get node address given a null parameter" in { client =>
    val nodeF = client.getNodeAddresses()

    nodeF.map { result =>
      assert(result.isEmpty)
    }
  }
}
