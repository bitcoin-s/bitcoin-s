package org.bitcoins.rpc.jsonmodels

import java.net.InetAddress

import org.bitcoins.core.crypto.{DoubleSha256Digest, ECPublicKey, Sha256Hash160Digest}
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.protocol.Address
import org.bitcoins.core.protocol.script.ScriptPubKey

sealed abstract class NetworkResult

case class ChainTip(
  height: Int,
  hash: DoubleSha256Digest,
  branchlen: Int,
  status: String) extends NetworkResult

case class GetNetworkInfoResult(
  version: Int,
  subversion: String,
  protocolversion: Int,
  localservices: String,
  localrelay: Boolean,
  timeoffset: Int,
  networkactive: Boolean,
  connections: Int,
  networks: Array[Network],
  relayfee: Bitcoins,
  incrementalfee: Bitcoins,
  localadresses: Option[Array[NetworkAddress]],
  warnings: String) extends NetworkResult

case class Network(
  name: String,
  limited: Boolean,
  reachable: Boolean,
  proxy: String,
  proxy_randomize_credentials: Boolean) extends NetworkResult

case class NetworkAddress(
  address: String,
  port: Int,
  score: Int) extends NetworkResult

case class GetBlockHeaderResult(
  hash: DoubleSha256Digest,
  confirmations: Int,
  height: Int,
  version: Int,
  versionHex: Int32,
  merkleroot: DoubleSha256Digest,
  time: UInt32,
  mediantime: UInt32,
  nonce: UInt32,
  bits: UInt32,
  difficulty: Double,
  chainwork: String,
  previousblockhash: Option[DoubleSha256Digest],
  nextblockhash: Option[DoubleSha256Digest]) extends NetworkResult

case class ValidateAddressResult(
                                  isvalid: Boolean,
                                  address: Option[Address],
                                  scriptPubKey: Option[ScriptPubKey],
                                  ismine: Option[Boolean],
                                  iswatchonly: Option[Boolean],
                                  isscript: Option[Boolean],
                                  script: Option[ScriptPubKey],
                                  hex: Option[String],
                                  addresses: Option[Array[Address]],
                                  sigrequired: Option[Int],
                                  pubkey: Option[ECPublicKey],
                                  iscompressed: Option[Boolean],
                                  account: Option[String],
                                  hdkeypath: Option[String],
                                  hdmasterkeyid: Option[Sha256Hash160Digest]
                                ) extends NetworkResult

case class NodeBan(
                    address: InetAddress,
                    banned_until: UInt32,
                    ban_created: UInt32,
                    ban_reason: String
                  ) extends NetworkResult

case class Node(
                 addednode: InetAddress,
                 connected: Option[Boolean],
                 addresses: Option[Array[NodeAddress]]
               ) extends NetworkResult
case class NodeAddress(
                        address: InetAddress,
                        connected: String
                      ) extends NetworkResult

// Is Double the correct type for priorities? PRIORITY
case class GetMemPoolEntryResult(
                                  size: Int,
                                  fee: Bitcoins,
                                  modifiedfee: Bitcoins,
                                  time: UInt32,
                                  height: Int,
                                  startingpriority: Double,
                                  currentpriority: Double,
                                  descendantcount: Int,
                                  descendantsize: Int,
                                  descendantfees: Int,
                                  ancestorcount: Int,
                                  ancestorsize: Int,
                                  ancestorfees: Int,
                                  depends: Option[Array[DoubleSha256Digest]]
                                ) extends NetworkResult

case class GetMemPoolInfoResult(
                                 size: Int,
                                 bytes: Int,
                                 usage: Int,
                                 maxmempool: Int,
                                 mempoolminfee: Bitcoins,
                                 minrelaytxfee: Bitcoins
                               ) extends NetworkResult

case class GetTxOutSetInfoResult(
                                  height: Int,
                                  bestblock: DoubleSha256Digest,
                                  transactions: Int,
                                  txouts: Int,
                                  bytes_serialized: Int,
                                  hash_serialized: DoubleSha256Digest,
                                  total_amount: Bitcoins
                                ) extends NetworkResult