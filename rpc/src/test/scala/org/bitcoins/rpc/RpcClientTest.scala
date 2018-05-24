package org.bitcoins.rpc

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.ActorMaterializerSettings
import org.bitcoins.core.util.BitcoinSLogger
import org.scalatest.{ AsyncFlatSpec, FlatSpec }

class RpcClientTest extends AsyncFlatSpec {
  implicit val system = ActorSystem()
  implicit val m = ActorMaterializer(ActorMaterializerSettings(system))
  implicit val ec = m.executionContext

  behavior of "RpcClient"
  it should "be able to get an address from bitcoind" in {
    val client = new RpcClient
    val addressF = client.getNewAddress(None)
    val logger = BitcoinSLogger.logger
    addressF.map { address =>
      logger.info(address.value)
      assert(true)
    }
  }
}
