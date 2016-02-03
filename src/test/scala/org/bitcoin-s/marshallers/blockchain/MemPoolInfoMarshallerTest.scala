package org.bitcoins.marshallers.blockchain

import org.bitcoins.protocol.blockchain.MemPoolInfo
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

  "MemPoolInfo" must "parse node's tx memory pool" in {
    val mempool : MemPoolInfo = MemPoolInfoMarshaller.MemPoolInfoFormatter.read(json)
      mempool.size must be (270)
      mempool.bytes must be (295151)
  }
}
