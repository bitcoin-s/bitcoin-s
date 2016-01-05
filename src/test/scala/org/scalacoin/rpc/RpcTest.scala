package org.scalacoin.rpc

import org.scalatest.{MustMatchers, FlatSpec}
import scala.sys.process._

/**
 * Created by Tom on 1/4/2016.
 */
class RpcTest extends FlatSpec with MustMatchers {
  "RpcTest" must "get blockcount from bitcoin-cli" in {
    val GetBlockCount = "bitcoin-cli -testnet getblockcount".!!
    GetBlockCount.toFloat.toInt must be >= (584301)
  }
}
