package org.scalacoin.marshallers.rpc.bitcoincore.blockchain.softforks

import org.scalacoin.rpc.bitcoincore.blockchain.softforks.{SoftForks}
import org.scalatest.{MustMatchers, FlatSpec}
import spray.json._

/**
 * Created by Tom on 1/11/2016.
 */
class SoftForksMarshallerTest extends FlatSpec with MustMatchers {
  val str =
    """
      | {
      |   "id" : "bip34",
      |   "version" : 2,
      |   "enforce" : {
      |       "status" : true,
      |       "found" : 100,
      |       "required" : 51,
      |       "window" : 100
      |   },
      |   "reject" : {
      |       "status" : true,
      |       "found" : 100,
      |       "required" : 75,
      |       "window" : 100
      |  }
      |}
    """.stripMargin

  val json = str.parseJson

  "SoftForkMarshaller" must "parse softfork information" in {
    val detail : SoftForks = SoftForkMarshaller.SoftForkFormatter.read(json)
      detail.id must be ("bip34")
      detail.version must be (2)
      detail.enforce.status must be (true)
      detail.enforce.newVersionBlocksFound must be (100)
      detail.enforce.requiredBlocks must be (51)
      detail.enforce.recentBlocksWindow must be (100)
      detail.reject.status must be (true)
      detail.reject.newVersionBlocksFound must be (100)
      detail.reject.requiredBlocks must be (75)
      detail.reject.recentBlocksWindow must be (100)
  }

  it must "write softfork info" in {
    val json = str.parseJson
    val detail : SoftForks = SoftForkMarshaller.SoftForkFormatter.read(json)
    val writtenSoftFork = SoftForkMarshaller.SoftForkFormatter.write(detail)
    writtenSoftFork.asJsObject.fields("id") must be (JsString("bip34"))
    writtenSoftFork.asJsObject.fields("version") must be (JsNumber(2))
  }
}
