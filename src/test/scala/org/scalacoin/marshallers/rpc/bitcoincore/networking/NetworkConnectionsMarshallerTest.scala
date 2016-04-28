package org.scalacoin.marshallers.rpc.bitcoincore.networking

import org.scalacoin.rpc.bitcoincore.networking.NetworkConnections
import org.scalatest.{FlatSpec, MustMatchers}
import spray.json._

/**
 * Created by Tom on 1/6/2016.
 */
class NetworkConnectionsMarshallerTest extends FlatSpec with MustMatchers {
  val str =
    """
      | {
      | "name" : "ipv4",
      | "limited" : false,
      | "reachable" : false,
      | "proxy" : "",
      | "proxy_randomize_credentials" : false
      | }
    """.stripMargin

  val json = str.parseJson
  val detail : NetworkConnections = NetworkConnectionsMarshaller.NetworkConnectionsFormatter.read(json)


  "NetworkConnections" must "parse network info connections" in {
    detail.name must be ("ipv4")
    detail.limited must be (false)
    detail.reachable must be (false)
    detail.proxy must be ("")
    detail.proxyRandomizeCredentials must be (false)
  }

  it must "write network connection info" in {
    val writtenNetworkConnections = NetworkConnectionsMarshaller.NetworkConnectionsFormatter.write(detail)
    writtenNetworkConnections.asJsObject.fields("name") must be (JsString("ipv4"))
    writtenNetworkConnections.asJsObject.fields("limited") must be (JsBoolean(false))
    writtenNetworkConnections.asJsObject.fields("reachable") must be (JsBoolean(false))
    writtenNetworkConnections.asJsObject.fields("proxy") must be (JsString(""))
    writtenNetworkConnections.asJsObject.fields("proxy_randomize_credentials") must be (JsBoolean(false))
  }
}
