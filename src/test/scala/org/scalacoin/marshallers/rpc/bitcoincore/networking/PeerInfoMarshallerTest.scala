package org.scalacoin.marshallers.rpc.bitcoincore.networking

import org.scalacoin.protocol.rpc.bitcoincore.networking.PeerInfo
import org.scalatest.{FlatSpec, MustMatchers}
import spray.json._

/**
 * Created by Tom on 1/6/2016.
 */
class PeerInfoMarshallerTest extends FlatSpec with MustMatchers {
  var str =
    """
      |{
      |        "id" : 20,
      |        "addr" : "158.85.83.84:18333",
      |        "addrlocal" : "173.26.115.141:63270",
      |        "services" : "0000000000000001",
      |        "lastsend" : 1452098899,
      |        "lastrecv" : 1452098899,
      |        "bytessent" : 3815198,
      |        "bytesrecv" : 10495519,
      |        "conntime" : 1452092808,
      |        "timeoffset" : 55,
      |        "pingtime" : 0.14842800,
      |        "version" : 70002,
      |        "subver" : "/Satoshi:0.9.3/",
      |        "inbound" : false,
      |        "startingheight" : 501654,
      |        "banscore" : 0,
      |        "synced_headers" : 495724,
      |        "synced_blocks" : -1,
      |        "inflight" : [ 9, 10
      |        ],
      |        "whitelisted" : false
      |    }
    """.stripMargin

  val json = str.parseJson
  val peer : PeerInfo = PeerInfoMarshaller.PeerInfoFormatter.read(json)

  "PeerInfoMarshaller" must "parse peer information" in {
    peer.id must be (20)
    peer.addr must be ("158.85.83.84:18333")
    peer.addrLocal must be ("173.26.115.141:63270")
    peer.services must be ("0000000000000001")
    peer.lastSend must be (1452098899)
    peer.lastRecv must be (1452098899)
    peer.bytesSent must be (3815198)
    peer.bytesRecv must be (10495519)
    peer.connTime must be (1452092808)
    peer.timeOffSet must be (55)
    peer.pingTime must be (0.14842800)
    peer.version must be (70002)
    peer.subVer must be ("/Satoshi:0.9.3/")
    peer.inbound must be (false)
    peer.startingHeight must be (501654)
    peer.banScore must be (0)
    peer.syncedHeaders must be (495724)
    peer.syncedBlocks must be (-1)
    peer.inFlight must be (Seq(9, 10))
    peer.whiteListed must be (false)
  }

  it must "write peer info" in {
    val writtenPeerInfo = PeerInfoMarshaller.PeerInfoFormatter.write(peer)
    writtenPeerInfo.asJsObject.fields("id") must be (JsNumber(20))
    writtenPeerInfo.asJsObject.fields("addr") must be (JsString("158.85.83.84:18333"))
    writtenPeerInfo.asJsObject.fields("addrlocal") must be (JsString("158.85.83.84:18333"))
    writtenPeerInfo.asJsObject.fields("services") must be (JsString("0000000000000001"))
    writtenPeerInfo.asJsObject.fields("lastsend") must be (JsNumber(1452098899))
    writtenPeerInfo.asJsObject.fields("lastrecv") must be (JsNumber(1452098899))
    writtenPeerInfo.asJsObject.fields("bytessent") must be (JsNumber(3815198))
    writtenPeerInfo.asJsObject.fields("bytesrecv") must be (JsNumber(10495519))
    writtenPeerInfo.asJsObject.fields("conntime") must be (JsNumber(1452092808))
    writtenPeerInfo.asJsObject.fields("timeoffset") must be (JsNumber(55))
    writtenPeerInfo.asJsObject.fields("pingtime") must be (JsNumber(0.14842800))
    writtenPeerInfo.asJsObject.fields("subver") must be (JsString("/Satoshi:0.9.3/"))
    writtenPeerInfo.asJsObject.fields("version") must be (JsNumber(70002))
    writtenPeerInfo.asJsObject.fields("inbound") must be (JsBoolean(false))
    writtenPeerInfo.asJsObject.fields("startingheight") must be (JsNumber(501654))
    writtenPeerInfo.asJsObject.fields("banscore") must be (JsNumber(0))
    writtenPeerInfo.asJsObject.fields("synced_headers") must be (JsNumber(495724))
    writtenPeerInfo.asJsObject.fields("synced_blocks") must be (JsNumber(-1))
    writtenPeerInfo.asJsObject.fields("inflight") must be (Seq(9,10))
    writtenPeerInfo.asJsObject.fields("whitelsited") must be (JsBoolean(false))
  }
}