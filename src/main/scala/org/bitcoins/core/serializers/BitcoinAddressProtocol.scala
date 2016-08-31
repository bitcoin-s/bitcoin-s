package org.bitcoins.core.serializers

import org.bitcoins.core.protocol.{Address, BitcoinAddress}
import spray.json._
/**
 * Created by chris on 12/19/15.
 */
object AddressProtocol extends DefaultJsonProtocol {

  implicit object AddressFormat extends RootJsonFormat[Address] {
    override def read(jsValue: JsValue) = {
      jsValue match {
        case JsString(string) => string match {
          case s if Seq('1','3','2','m','n').contains(s(0)) => BitcoinAddress(s)
          case _ => throw new RuntimeException("Addresses should always start with 'a' '1' or '3'")
        }
        case _ => throw new RuntimeException("Addresses should always be represented by a JsString")
      }
    }

    override def write(address: Address) = {
      val m: Map[String, JsValue] = Map("address" -> JsString(address.value))
      JsObject(m)
    }
  }
}
