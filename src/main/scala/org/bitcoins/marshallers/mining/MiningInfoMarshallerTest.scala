package org.bitcoins.marshallers.mining

import org.bitcoins.protocol.mining.GetMiningInfo
import org.scalatest.{FlatSpec, MustMatchers}
import spray.json._

/**
 * Created by Tom on 1/13/2016.
 */
class MiningInfoMarshallerTest extends FlatSpec with MustMatchers {
  val str =
    """
      |{
      |    "blocks" : 633027,
      |    "currentblocksize" : 0,
      |    "currentblocktx" : 0,
      |    "difficulty" : 1.00000000,
      |    "errors" : "",
      |    "genproclimit" : -1,
      |    "networkhashps" : 2302003138855,
      |    "pooledtx" : 78,
      |    "testnet" : true,
      |    "chain" : "test",
      |    "generate" : false
      |}
    """.stripMargin

  val json = str.parseJson

  "MiningInfoMarshaller" must "parse mining information" in {
    val miningMeta : GetMiningInfo = MiningInfoMarshaller.MiningInfoFormatter.read(json)
    miningMeta.blocks must be (633027)
    miningMeta.currentBlockSize must be (0)
    miningMeta.currentBlockTx must be (0)
    miningMeta.difficulty must be (1.00000000)
    miningMeta.errors must be ("")
    miningMeta.genProcLimit must be (-1)
    miningMeta.networkHashPerSecond must be (BigInt("2302003138855"))
    miningMeta.pooledTx must be (78)
    miningMeta.testNet must be (true)
    miningMeta.chain must be ("test")
    miningMeta.generate must be (false)
  }
}
