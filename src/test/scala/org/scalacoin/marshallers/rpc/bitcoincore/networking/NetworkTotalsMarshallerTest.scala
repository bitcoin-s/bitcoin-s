package org.scalacoin.marshallers.rpc.bitcoincore.networking

import org.scalacoin.rpc.bitcoincore.networking.NetworkTotals
import org.scalatest.{FlatSpec, MustMatchers}
import spray.json._

import scala.math.BigInt

/**
 * Created by Tom on 1/8/2016.
 */
class NetworkTotalsMarshallerTest extends FlatSpec with MustMatchers {
  val str =
    """
      |{
      |    "totalbytesrecv" : 1972832211,
      |    "totalbytessent" : 71483458,
      |    "timemillis" : 1452264952971
      |}
    """.stripMargin

  val json = str.parseJson
  val NetworkDetail : NetworkTotals = NetworkTotalsMarshaller.NetworkTotalFormatter.read(json)

  "NetworkTotals" must "parse network total infos" in {
    NetworkDetail.totalBytesRecv must be (1972832211)
    NetworkDetail.totalBytesSent must be (71483458)
    NetworkDetail.timeInMilliSeconds must be (BigInt("1452264952971"))
  }

  it must "write network total info" in {
    val writtenNetworkTotal = NetworkTotalsMarshaller.NetworkTotalFormatter.write(NetworkDetail)
    writtenNetworkTotal.asJsObject.fields("totalbytesrecv") must be (JsNumber(1972832211))
    writtenNetworkTotal.asJsObject.fields("totalbytessent") must be (JsNumber(71483458))
    writtenNetworkTotal.asJsObject.fields("timemillis") must be (JsNumber(BigInt("1452264952971")))

  }
}
