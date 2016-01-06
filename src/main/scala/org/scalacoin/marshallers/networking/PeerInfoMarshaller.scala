package org.scalacoin.marshallers.networking

import org.scalacoin.marshallers.MarshallerUtil
import org.scalacoin.protocol.networking.{PeerInfo, PeerInfoImpl}
import spray.json._

/**
 * Created by Tom on 1/6/2016.
 */
object PeerInfoMarshaller extends DefaultJsonProtocol with MarshallerUtil {
  val idKey = "id"
  val addrKey = "addr"
  val addrLocalKey = "addrlocal"
  val servicesKey = "services"
  val lastSendKey = "lastsend"
  val lastRecvKey = "lastrecv"
  val bytesSentKey = "bytessent"
  val bytesRecvKey = "bytesrecv"
  val connectionTimeKey = "conntime"
  val timeOffSetKey = "timeoffset"
  val pingTimeKey = "pingtime"
  val versionKey = "version"
  val subVersionKey = "subver"
  val inboundKey = "inbound"
  val startingHeightKey = "startingheight"
  val banScoreKey = "banscore"
  val syncedHeadersKey = "synced_headers"
  val syncedBlocksKey = "synced_blocks"
  val inFlightKey = "inflight"
  val whiteListedKey = "whitelisted"

  implicit object PeerInfoFormatter extends RootJsonFormat[PeerInfo] {
    override def read(value: JsValue): PeerInfo = {
      val obj = value.asJsObject
      val id = obj.fields(idKey).convertTo[Int]
      val addr = obj.fields(addrKey).convertTo[String]
      val addrLocal = obj.fields(addrLocalKey).convertTo[String]
      val services = obj.fields(servicesKey).convertTo[String]
      val lastSend = obj.fields(lastSendKey).convertTo[Long]
      val lastRecv = obj.fields(lastRecvKey).convertTo[Long]
      val bytesSent = obj.fields(bytesSentKey).convertTo[Long]
      val bytesRecv = obj.fields(bytesRecvKey).convertTo[Long]
      val connTime = obj.fields(connectionTimeKey).convertTo[Long]
      val timeOffSet = obj.fields(timeOffSetKey).convertTo[Int]
      val pingTime = obj.fields(pingTimeKey).convertTo[Double]
      val version = obj.fields(versionKey).convertTo[Long]
      val subVer = obj.fields(subVersionKey).convertTo[String]
      val inbound = obj.fields(inboundKey).convertTo[Boolean]
      val startingHeight = obj.fields(startingHeightKey).convertTo[Int]
      val banScore = obj.fields(banScoreKey).convertTo[Int]
      val syncedHeaders = obj.fields(syncedHeadersKey).convertTo[Int]
      val syncedBlocks = obj.fields(syncedBlocksKey).convertTo[Int]
      val inFlight = obj.fields(inFlightKey).convertTo[Seq[Int]]
      val whiteListed = obj.fields(whiteListedKey).convertTo[Boolean]
      PeerInfoImpl(id, addr, addrLocal, services, lastSend, lastRecv, bytesSent, bytesRecv, connTime, timeOffSet, pingTime, version, subVer, inbound, startingHeight, banScore, syncedHeaders, syncedBlocks, inFlight, whiteListed)
    }

    override def write (peer : PeerInfo) : JsValue = {
      val inFlight : JsArray = convertToJsArray(peer.inFlight)
      val m : Map[String, JsValue] = Map (
        idKey -> JsNumber(peer.id),
        addrKey -> JsString(peer.addr),
        addrLocalKey -> JsString(peer.addrLocal),
        servicesKey -> JsString(peer.services),
        lastSendKey -> JsNumber(peer.lastSend),
        lastRecvKey -> JsNumber(peer.lastRecv),
        bytesSentKey -> JsNumber(peer.bytesSent),
        bytesRecvKey -> JsNumber(peer.bytesRecv),
        connectionTimeKey -> JsNumber(peer.connTime),
        pingTimeKey -> JsNumber(peer.pingTime),
        versionKey -> JsNumber(peer.version),
        subVersionKey -> JsString(peer.subVer),
        inboundKey -> JsBoolean(peer.inbound),
        startingHeightKey -> JsNumber(peer.startingHeight),
        banScoreKey -> JsNumber(peer.banScore),
        syncedHeadersKey -> JsNumber(peer.syncedHeaders),
        syncedBlocksKey -> JsNumber(peer.syncedBlocks),
        inFlightKey ->  inFlight,
        whiteListedKey -> JsBoolean(peer.whiteListed)
        )
      JsObject(m)
    }
  }
}