package org.bitcoins.marshallers.networking

import org.scalatest.{MustMatchers, FlatSpec}
import spray.json._
import org.bitcoins.protocol.networking.{NetworkInfo, NetworkInfoImpl}
import org.bitcoins.marshallers.networking.{NetworkMarshaller}

/**
 * Created by Tom on 1/6/2016.
 */
class NetworkInfoMarshallerTest extends FlatSpec with MustMatchers{
  val str =
    """
      |{
      |    "version" : 110200,
      |    "subversion" : "/Satoshi:0.11.2/",
      |    "protocolversion" : 70002,
      |    "localservices" : "0000000000000001",
      |    "timeoffset" : 2,
      |    "connections" : 8,
      |    "networks" : [
      |        {
      |            "name" : "ipv4",
      |            "limited" : false,
      |            "reachable" : false,
      |            "proxy" : "",
      |            "proxy_randomize_credentials" : false
      |        },
      |        {
      |            "name" : "ipv6",
      |            "limited" : false,
      |            "reachable" : false,
      |            "proxy" : "",
      |            "proxy_randomize_credentials" : false
      |        },
      |        {
      |            "name" : "onion",
      |            "limited" : false,
      |            "reachable" : false,
      |            "proxy" : "",
      |            "proxy_randomize_credentials" : false
      |        }
      |    ],
      |    "relayfee" : 0.00005000,
      |    "localaddresses" : [
      |    ]
      |}
    """.stripMargin

  val json = str.parseJson

  "NetworkMarshaller" must "parse network information" in {
    val network : NetworkInfo = NetworkMarshaller.NetworkInfoFormatter.read(json)
    network.version must be (110200)
    network.subVersion must be ("/Satoshi:0.11.2/")
    network.protocolVersion must be (70002)
    network.localServices must be ("0000000000000001")
    network.timeOffSet must be (2)
    network.connections must be (8)
    network.networks.size must be (3)
    network.relayFee must be (0.00005000)
    network.localAddresses must be (Seq())
  }

}
