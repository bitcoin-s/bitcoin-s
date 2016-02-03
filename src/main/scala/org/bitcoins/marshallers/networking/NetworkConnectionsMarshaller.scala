package org.bitcoins.marshallers.networking

import org.bitcoins.protocol.networking.{NetworkConnections, NetworkConnectionsImpl}
import spray.json._

/**
 * Created by Tom on 1/6/2016.
 */
object NetworkConnectionsMarshaller extends DefaultJsonProtocol {
  val nameKey = "name"
  val limitedKey = "limited"
  val reachableKey = "reachable"
  val proxyKey = "proxy"
  val proxyRandomizeCredentialsKey = "proxy_randomize_credentials"

  implicit object NetworkConnectionsFormatter extends RootJsonFormat[NetworkConnections] {
    override def read(value : JsValue) : NetworkConnections = {
      val obj = value.asJsObject
      val name = obj.fields(nameKey).convertTo[String]
      val limited = obj.fields(limitedKey).convertTo[Boolean]
      val reachable = obj.fields(reachableKey).convertTo[Boolean]
      val proxy = obj.fields(proxyKey).convertTo[String]
      val proxyRandomizeCredentials = obj.fields(proxyRandomizeCredentialsKey).convertTo[Boolean]
      NetworkConnectionsImpl(name, limited, reachable, proxy, proxyRandomizeCredentials)
    }

    override def write(detail : NetworkConnections) : JsValue = {
      val m : Map[String, JsValue] = Map (
        nameKey -> JsString(detail.name),
        limitedKey -> JsBoolean(detail.limited),
        reachableKey -> JsBoolean(detail.reachable),
        proxyKey -> JsString(detail.proxy),
        proxyRandomizeCredentialsKey -> JsBoolean(detail.proxyRandomizeCredentials)
      )
      JsObject(m)
    }
  }
}