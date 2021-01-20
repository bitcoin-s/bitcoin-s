package org.bitcoins.commons.jsonmodels.bitcoind

import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.number.{UInt32, UInt64}
import org.bitcoins.core.p2p.ServiceIdentifier
import org.bitcoins.core.wallet.fee.SatoshisPerKiloByte

import java.net.URI
import scala.concurrent.duration.FiniteDuration

sealed abstract class NetworkResult

case class Node(
    addednode: URI,
    connected: Option[Boolean],
    addresses: Option[Vector[NodeAddress]])
    extends NetworkResult

case class NodeAddress(address: URI, connected: String) extends NetworkResult

case class GetNetTotalsResult(
    totalbytesrecv: Int,
    totalbytessent: Int,
    timemillis: UInt64,
    uploadtarget: NetTarget)
    extends NetworkResult

case class NetTarget(
    timeframe: UInt32,
    target: Int,
    target_reached: Boolean,
    serve_historical_blocks: Boolean,
    bytes_left_in_cycle: Int,
    time_left_in_cycle: UInt32)
    extends NetworkResult

trait GetNetworkInfoResult extends NetworkResult {
  def version: Int
  def subversion: String
  def protocolversion: Int
  def localservices: String
  def localservicesnames: Option[Vector[ServiceIdentifier]]
  def localrelay: Boolean
  def timeoffset: Int
  def networkactive: Boolean
  def connections: Int
  def networks: Vector[Network]
  def relayfee: Bitcoins
  def incrementalfee: Bitcoins
  def localadresses: Option[Vector[NetworkAddress]]
  def warnings: String
}

case class GetNetworkInfoResultPreV21(
    version: Int,
    subversion: String,
    protocolversion: Int,
    localservices: String,
    localservicesnames: Option[Vector[ServiceIdentifier]],
    localrelay: Boolean,
    timeoffset: Int,
    networkactive: Boolean,
    connections: Int,
    networks: Vector[Network],
    relayfee: Bitcoins,
    incrementalfee: Bitcoins,
    localadresses: Option[Vector[NetworkAddress]],
    warnings: String)
    extends GetNetworkInfoResult

case class GetNetworkInfoResultPostV21(
    version: Int,
    subversion: String,
    protocolversion: Int,
    localservices: String,
    localservicesnames: Option[Vector[ServiceIdentifier]],
    localrelay: Boolean,
    timeoffset: Int,
    networkactive: Boolean,
    connections: Int,
    connections_in: Int,
    connections_out: Int,
    networks: Vector[Network],
    relayfee: Bitcoins,
    incrementalfee: Bitcoins,
    localadresses: Option[Vector[NetworkAddress]],
    warnings: String)
    extends GetNetworkInfoResult

case class Network(
    name: String,
    limited: Boolean,
    reachable: Boolean,
    proxy: String,
    proxy_randomize_credentials: Boolean)
    extends NetworkResult

case class NetworkAddress(address: String, port: Int, score: Int)
    extends NetworkResult

sealed trait Peer extends NetworkResult {
  def id: Int
  def networkInfo: PeerNetworkInfo
  def version: Int
  def subver: String
  def inbound: Boolean
  def addnode: Boolean
  def startingheight: Int
  def synced_headers: Int
  def synced_blocks: Int
  def inflight: Vector[Int]
  def bytessent_per_msg: Map[String, Int]
  def bytesrecv_per_msg: Map[String, Int]
  def minfeefilter: Option[SatoshisPerKiloByte]
}

case class PeerPreV20(
    id: Int,
    networkInfo: PeerNetworkInfoPreV21,
    version: Int,
    subver: String,
    inbound: Boolean,
    addnode: Boolean,
    startingheight: Int,
    banscore: Int,
    synced_headers: Int,
    synced_blocks: Int,
    inflight: Vector[Int],
    whitelisted: Boolean,
    bytessent_per_msg: Map[String, Int],
    bytesrecv_per_msg: Map[String, Int],
    minfeefilter: Option[SatoshisPerKiloByte])
    extends Peer

case class PeerV20(
    id: Int,
    networkInfo: PeerNetworkInfoPreV21,
    version: Int,
    subver: String,
    inbound: Boolean,
    addnode: Boolean,
    startingheight: Int,
    synced_headers: Int,
    synced_blocks: Int,
    inflight: Vector[Int],
    whitelisted: Boolean,
    bytessent_per_msg: Map[String, Int],
    bytesrecv_per_msg: Map[String, Int],
    minfeefilter: Option[SatoshisPerKiloByte])
    extends Peer

case class PeerPostV21(
    id: Int,
    networkInfo: PeerNetworkInfoPostV21,
    version: Int,
    subver: String,
    inbound: Boolean,
    connection_type: String,
    startingheight: Int,
    synced_headers: Int,
    synced_blocks: Int,
    inflight: Vector[Int],
    bytessent_per_msg: Map[String, Int],
    bytesrecv_per_msg: Map[String, Int],
    minfeefilter: Option[SatoshisPerKiloByte])
    extends Peer {
  override val addnode: Boolean = connection_type == "manual"
}

trait PeerNetworkInfo extends NetworkResult {
  def addr: URI
  def addrbind: URI
  def addrlocal: Option[URI]
  def services: String
  def servicesnames: Option[Vector[ServiceIdentifier]]
  def relaytxes: Boolean
  def lastsend: UInt32
  def lastrecv: UInt32
  def bytessent: Int
  def bytesrecv: Int
  def conntime: UInt32
  def timeoffset: Int
  def pingtime: Option[BigDecimal]
  def minping: Option[BigDecimal]
  def pingwait: Option[BigDecimal]
}

case class PeerNetworkInfoPreV21(
    addr: URI,
    addrbind: URI,
    addrlocal: Option[URI],
    services: String,
    servicesnames: Option[Vector[ServiceIdentifier]],
    relaytxes: Boolean,
    lastsend: UInt32,
    lastrecv: UInt32,
    bytessent: Int,
    bytesrecv: Int,
    conntime: UInt32,
    timeoffset: Int,
    pingtime: Option[BigDecimal],
    minping: Option[BigDecimal],
    pingwait: Option[BigDecimal])
    extends PeerNetworkInfo

case class PeerNetworkInfoPostV21(
    addr: URI,
    addrbind: URI,
    addrlocal: Option[URI],
    network: String,
    mapped_as: Option[Int],
    services: String,
    servicesnames: Option[Vector[ServiceIdentifier]],
    relaytxes: Boolean,
    lastsend: UInt32,
    lastrecv: UInt32,
    last_transaction: UInt32,
    last_block: UInt32,
    bytessent: Int,
    bytesrecv: Int,
    conntime: UInt32,
    timeoffset: Int,
    pingtime: Option[BigDecimal],
    minping: Option[BigDecimal],
    pingwait: Option[BigDecimal])
    extends PeerNetworkInfo

trait NodeBan extends NetworkResult {
  def address: URI
  def banned_until: UInt32
  def ban_created: UInt32
}

case class NodeBanPreV20(
    address: URI,
    banned_until: UInt32,
    ban_created: UInt32,
    ban_reason: String)
    extends NodeBan

case class NodeBanPostV20(
    address: URI,
    banned_until: UInt32,
    ban_created: UInt32)
    extends NodeBan

final case class GetNodeAddressesResult(
    time: FiniteDuration,
    services: Int,
    address: java.net.URI,
    port: Int
) extends NetworkResult
