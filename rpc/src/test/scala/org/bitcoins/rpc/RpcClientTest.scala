package org.bitcoins.rpc

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import org.bitcoins.core.util.BitcoinSLogger
import org.scalatest.AsyncFlatSpec

class RpcClientTest extends AsyncFlatSpec {
  implicit val system = ActorSystem()
  implicit val m = ActorMaterializer()
  implicit val ec = m.executionContext

  val client = new RpcClient
  val logger = BitcoinSLogger.logger

  behavior of "RpcClient"

  it should "be able to get an address from bitcoind" in {
    val addressF = client.getNewAddress(None)
    addressF.map { address =>
      logger.info(address.value)
      assert(true)
    }
  }

  it should "be able to get an address from bitcoind given an account" in {
    val addressF = client.getNewAddress(Some(""))
    addressF.map { address =>
      logger.info(address.value)
      assert(true)
    }
  }

  it should "be able to get the block count" in {
    val blockCountF = client.getBlockCount
    blockCountF.map { count =>
      logger.info(count.toString)
      assert(true)
    }
  }

  it should "be able to get the connection count" in {
    val connectionCountF = client.getConnectionCount
    connectionCountF.map { count =>
      logger.info(count.toString)
      assert(true)
    }
  }

  it should "be able to get the best block hash" in {
    val bestHashF = client.getBestBlockHash
    bestHashF.map { hash =>
      logger.info(hash)
      assert(true)
    }
  }
}
