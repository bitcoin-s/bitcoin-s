package org.bitcoins.rpc.common

import org.bitcoins.core.number.UInt32
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.testkit.util.BitcoindRpcTest

import scala.concurrent.Future
import org.bitcoins.rpc.BitcoindException.MiscError

class NodeRpcTest extends BitcoindRpcTest {
  lazy val clientF: Future[BitcoindRpcClient] =
    BitcoindRpcTestUtil.startedBitcoindRpcClient(clientAccum = clientAccum)

  behavior of "NodeRpc"

  it should "be able to abort a rescan of the blockchain" in {
    clientF.flatMap { client =>
      // generate some extra blocks so rescan isn't too quick
      client.generate(3000).flatMap { _ =>
        val rescanFailedF =
          recoverToSucceededIf[MiscError](client.rescanBlockChain())
        client.abortRescan().flatMap { _ =>
          rescanFailedF
        }
      }
    }
  }

  it should "be able to ping" in {
    for {
      client <- clientF
      _ <- client.ping()
    } yield succeed
  }

  it should "be able to get and set the logging configuration" in {
    for {
      client <- clientF
      info <- client.logging
      infoNoQt <- client.logging(exclude = Vector("qt"))
    } yield {
      info.keySet.foreach(category => assert(info(category)))
      assert(!infoNoQt("qt"))
    }
  }

  it should "be able to get the memory info" in {
    for {
      client <- clientF
      info <- client.getMemoryInfo
    } yield {
      assert(info.locked.used > 0)
      assert(info.locked.free > 0)
      assert(info.locked.total > 0)
      assert(info.locked.locked > 0)
      assert(info.locked.chunks_used > 0)
    }
  }

  it should "be able to get the client's uptime" in {
    for {
      client <- clientF
      time <- client.uptime
    } yield assert(time > UInt32(0))
  }

  it should "be able to get help from bitcoind" in {
    for {
      client <- clientF
      genHelp <- client.help()
      helpHelp <- client.help("help")
    } yield {
      assert(!genHelp.isEmpty)
      assert(genHelp != helpHelp)
      assert(!helpHelp.isEmpty)
    }
  }
}
