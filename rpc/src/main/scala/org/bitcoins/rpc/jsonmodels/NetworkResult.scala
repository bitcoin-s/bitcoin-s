package org.bitcoins.rpc.jsonmodels

import java.net.URI

import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.number.{UInt32, UInt64}

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

case class GetNetworkInfoResult(
    version: Int,
    subversion: String,
    protocolversion: Int,
    localservices: String,
    localrelay: Boolean,
    timeoffset: Int,
    networkactive: Boolean,
    connections: Int,
    networks: Vector[Network],
    relayfee: Bitcoins,
    incrementalfee: Bitcoins,
    localadresses: Option[Vector[NetworkAddress]],
    warnings: String)
    extends NetworkResult

case class Network(
    name: String,
    limited: Boolean,
    reachable: Boolean,
    proxy: String,
    proxy_randomize_credentials: Boolean)
    extends NetworkResult

case class NetworkAddress(address: String, port: Int, score: Int)
    extends NetworkResult

case class Peer(
    id: Int,
    networkInfo: PeerNetworkInfo,
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
    bytesrecv_per_msg: Map[String, Int])
    extends NetworkResult

case class PeerNetworkInfo(
    addr: URI,
    addrbind: URI,
    addrlocal: Option[URI],
    services: String,
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
    extends NetworkResult

case class NodeBan(
    address: URI,
    banned_until: UInt32,
    ban_created: UInt32,
    ban_reason: String)
    extends NetworkResult
