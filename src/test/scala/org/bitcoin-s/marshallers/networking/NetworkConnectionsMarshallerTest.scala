package org.bitcoins.marshallers.networking

import org.bitcoins.protocol.networking.{NetworkConnections}
import org.scalatest.{MustMatchers, FlatSpec}
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

  "NetworkConnections" must "parse network info connections" in {
    val detail : NetworkConnections = NetworkConnectionsMarshaller.NetworkConnectionsFormatter.read(json)
    detail.name must be ("ipv4")
    detail.limited must be (false)
    detail.reachable must be (false)
    detail.proxy must be ("")
    detail.proxyRandomizeCredentials must be (false)
  }
}
