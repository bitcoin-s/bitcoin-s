package org.scalacoin.marshallers

import org.scalacoin.protocol.BitcoinAddress
import spray.json.{JsonWriter, JsArray, DefaultJsonProtocol, JsValue}
import scala.collection.breakOut

/**
 * Created by chris on 12/27/15.
 */
trait MarshallerUtil {
  def convertToAddressList(value : JsValue) : Seq[BitcoinAddress] = {
    import DefaultJsonProtocol._
    value match {
      case ja: JsArray => {
        ja.elements.toList.map(
          e => BitcoinAddress(e.convertTo[String]))
      }
      case _ => throw new RuntimeException("This Json type is not valid for parsing a list of bitcoin addresses")
    }
  }

  def convertToJsArray[T](addresses : Seq[T])(implicit formatter : JsonWriter[T]) : JsArray  = {
    JsArray(addresses.map(p =>
      formatter.write(p))(breakOut): Vector[JsValue])
  }

}
