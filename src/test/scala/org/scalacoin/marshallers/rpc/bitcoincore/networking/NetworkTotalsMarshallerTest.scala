package org.scalacoin.marshallers.rpc.bitcoincore.networking

import org.scalacoin.protocol.rpc.bitcoincore.networking.NetworkTotals
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

  "NetworkTotals" must "parse network total infos" in {
    val NetworkDetail : NetworkTotals = NetworkTotalsMarshaller.NetworkTotalFormatter.read(json)
    NetworkDetail.totalBytesRecv must be (1972832211)
    NetworkDetail.totalBytesSent must be (71483458)
    NetworkDetail.timeInMilliSeconds must be (BigInt("1452264952971"))
  }
}
