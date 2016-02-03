package org.bitcoins.marshallers.networking

import org.bitcoins.protocol.networking.PeerInfo
import org.scalatest.{MustMatchers, FlatSpec}
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

  "PeerInfoMarshaller" must "parse peer information" in {
    val peer : PeerInfo = PeerInfoMarshaller.PeerInfoFormatter.read(json)
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
}
