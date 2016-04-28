package org.bitcoins.marshallers.blockchain.softforks

import org.bitcoins.protocol.blockchain.softforks.{SoftForks}
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

  }
}
