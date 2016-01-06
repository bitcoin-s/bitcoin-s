package org.scalacoin.rpc

import org.scalatest.{MustMatchers, FlatSpec}
import scala.sys.process._

/**
 * Created by Tom on 1/4/2016.
 */
class RpcTest extends FlatSpec with MustMatchers {

  var TestNetString = "bitcoin-cli -testnet "
  "RpcTest" must "get blockcount from bitcoin-cli" in {
    val GetBlockCount = TestNetString + "getblockcount".!!
    GetBlockCount.toFloat.toInt must be >= (584301)
  }


}
