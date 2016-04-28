package org.bitcoins.marshallers

import org.bitcoins.protocol.{AssetAddress, Address, BitcoinAddress}
import spray.json._
/**
 * Created by chris on 12/19/15.
 */
object BitcoinAddressProtocol extends DefaultJsonProtocol {

  implicit val bitcoinAddressFormat = jsonFormat(BitcoinAddress.apply _, "value")

}

object AddressProtocol extends DefaultJsonProtocol {

  implicit object AddressFormat extends RootJsonFormat[Address] {
    override def read(jsValue: JsValue) = {
      jsValue match {
        case JsString(string) => string match {
          case s if s(0) == 'a' => AssetAddress(s)
          case s if s(0) == '1' || s(0) == '3' => BitcoinAddress(s)
          case _ => throw new RuntimeException("Addresses should always start with 'a' '1' or '3'")
        }
        case _ => throw new RuntimeException("Addresses should always be reprsented by a JsString")
      }
    }

    override def write(address: Address) = {
      val m: Map[String, JsValue] = Map("address" -> JsString(address.value))
      JsObject(m)
    }
  }
}
