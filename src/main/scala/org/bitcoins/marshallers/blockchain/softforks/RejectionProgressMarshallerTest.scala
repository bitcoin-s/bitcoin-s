package org.bitcoins.marshallers.blockchain.softforks

import org.bitcoins.protocol.blockchain.softforks.RejectionProgress
import org.scalatest.{MustMatchers, FlatSpec}
import spray.json._

/**
 * Created by Tom on 1/11/2016.
 */
class RejectionProgressMarshallerTest extends FlatSpec with MustMatchers {
  val str =
    """
      |{
      |  "status" : true,
      |  "found" : 100,
      |  "required" : 75,
      |  "window" : 100
      |}
    """.stripMargin

  val json = str.parseJson

  "RejectionProgress" must "parse progress for rejecting softforks" in {
    val reject : RejectionProgress = RejectionProgressMarshaller.RejectionProgressFormatter.read(json)
    reject.status must be (true)
    reject.newVersionBlocksFound must be (100)
    reject.requiredBlocks must be (75)
    reject.recentBlocksWindow must be (100)
  }
}
