package org.scalacoin.marshallers.rpc.bitcoincore.networking

import org.scalacoin.protocol.rpc.bitcoincore.networking.NetworkInfo
import org.scalatest.{FlatSpec, MustMatchers}
import spray.json._

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
  val network : NetworkInfo = NetworkMarshaller.NetworkInfoFormatter.read(json)

  "NetworkMarshaller" must "parse network information" in {
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

  it must "write network info" in {
    val writtenNetworkInfo = NetworkMarshaller.NetworkInfoFormatter.write(network)
    writtenNetworkInfo.asJsObject.fields("version") must be (JsNumber(110200))
    writtenNetworkInfo.asJsObject.fields("subversion") must be (JsString("/Satoshi:0.11.2/"))
    writtenNetworkInfo.asJsObject.fields("protocolversion") must be (JsNumber(70002))
    writtenNetworkInfo.asJsObject.fields("localservices") must be (JsString("0000000000000001"))
    writtenNetworkInfo.asJsObject.fields("timeoffset") must be (JsNumber(2))
    writtenNetworkInfo.asJsObject.fields("connections") must be (JsNumber(8))
    writtenNetworkInfo.asJsObject.fields("relayfee") must be (JsNumber(0.00005000))
    writtenNetworkInfo.asJsObject.fields("localaddresses") must be (JsArray())

  }

}
