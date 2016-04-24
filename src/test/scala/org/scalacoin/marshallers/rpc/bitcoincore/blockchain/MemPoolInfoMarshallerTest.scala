package org.scalacoin.marshallers.rpc.bitcoincore.blockchain

import org.scalacoin.protocol.rpc.bitcoincore.blockchain.MemPoolInfo
import org.scalatest.{MustMatchers, FlatSpec}
import spray.json._

/**
 * Created by Tom on 1/11/2016.
 */
class MemPoolInfoMarshallerTest extends FlatSpec with MustMatchers {
  val str =
    """
      |{
      |    "size" : 270,
      |    "bytes" : 295151
      |}
    """.stripMargin

  val json = str.parseJson
  val mempool : MemPoolInfo = MemPoolInfoMarshaller.MemPoolInfoFormatter.read(json)

  "MemPoolInfo" must "parse node's tx memory pool" in {
      mempool.size must be (270)
      mempool.bytes must be (295151)
  }

  it must "write mempool info" in {
    val writtenMemPool = MemPoolInfoMarshaller.MemPoolInfoFormatter.write(mempool)
    writtenMemPool.asJsObject.fields("size") must be (JsNumber(270))
    writtenMemPool.asJsObject.fields("bytes") must be (JsNumber(295151))
  }
}
