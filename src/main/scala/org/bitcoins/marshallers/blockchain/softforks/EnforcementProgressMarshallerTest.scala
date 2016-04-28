package org.bitcoins.marshallers.blockchain.softforks

import org.bitcoins.protocol.blockchain.softforks.EnforcementProgress
import org.scalatest.{FlatSpec, MustMatchers}
import spray.json._

/**
 * Created by Tom on 1/11/2016.
 */
class EnforcementProgressMarshallerTest extends FlatSpec with MustMatchers {
  val str =
    """
      |{
      |  "status" : true,
      |  "found" : 100,
      |  "required" : 51,
      |  "window" : 100
      |}
    """.stripMargin

  val json = str.parseJson

  "EnforcementProgress" must "parse progress for enforcing softforks" in {
    val enforce : EnforcementProgress = EnforcementProgressMarshaller.EnforcementProgressFormatter.read(json)
      enforce.status must be (true)
      enforce.newVersionBlocksFound must be (100)
      enforce.requiredBlocks must be (51)
      enforce.recentBlocksWindow must be (100)
  }
}
