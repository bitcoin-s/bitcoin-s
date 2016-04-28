package org.scalacoin.marshallers.rpc.bitcoincore.blockchain

import org.scalatest.{FlatSpec, MustMatchers}


/**
 * Created by Tom on 1/11/2016.
 */
class GetBlockCountTest extends FlatSpec with MustMatchers{
  val blockCount = "632529"


  "GetBlockCount" must "return block count" in {
    blockCount.toInt must be (632529)
  }
}
