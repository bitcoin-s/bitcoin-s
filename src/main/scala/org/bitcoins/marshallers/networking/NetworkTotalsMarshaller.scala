package org.bitcoins.marshallers.networking

import org.bitcoins.protocol.networking
import org.bitcoins.protocol.networking.{NetworkTotals, NetworkTotalsImpl}
import scala.math.BigInt
import spray.json._

/**
 * Created by Tom on 1/8/2016.
 */
object NetworkTotalsMarshaller extends DefaultJsonProtocol {
  val totalBytesRecvKey = "totalbytesrecv"
  val totalBytesSentKey = "totalbytessent"
  val timeInMilliSecondsKey = "timemillis"

  implicit object NetworkTotalFormatter extends RootJsonFormat[NetworkTotals] {
    override def read (value: JsValue) : NetworkTotals = {
      val obj = value.asJsObject
      val totalBytesRecv = obj.fields(totalBytesRecvKey).convertTo[Int]
      val totalBytesSent = obj.fields(totalBytesSentKey).convertTo[Int]
      val timeInMilliSeconds = obj.fields(timeInMilliSecondsKey).convertTo[BigInt]
      NetworkTotalsImpl(totalBytesRecv, totalBytesSent, timeInMilliSeconds)
    }

    override def write (NetworkDetail : NetworkTotals) : JsValue = {
      val m : Map[String, JsValue] = Map (
        totalBytesRecvKey -> JsNumber(NetworkDetail.totalBytesRecv),
        totalBytesSentKey -> JsNumber(NetworkDetail.totalBytesSent),
        timeInMilliSecondsKey -> JsNumber(NetworkDetail.timeInMilliSeconds)
      )
      JsObject(m)
    }
  }
}
