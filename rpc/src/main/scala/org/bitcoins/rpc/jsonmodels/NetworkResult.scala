package org.bitcoins.rpc.jsonmodels

import java.net.InetAddress

import org.bitcoins.core.crypto.DoubleSha256Digest
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
                                  script: Option[String],
                                  hex: Option[String],
                                  addresses: Option[Array[Address]],
                                  sigrequired: Option[Int],
                                  pubkey: Option[String], // Is this right?
                                  iscompressed: Option[Boolean],
                                  account: Option[String],
                                  hdkeypath: Option[String],
                                  hdmasterkeyid: Option[String] // Is this right?
                                ) extends NetworkResult

case class NodeBan(
                    address: InetAddress,
                    banned_until: UInt32,
                    ban_created: UInt32,
                    ban_reason: String
                  ) extends NetworkResult

case class Node(
                 addednode: InetAddress, // Need to add Reads[InetAddress]
                 connected: Option[Boolean],
                 addresses: Option[Array[NodeAddress]]
               ) extends NetworkResult
case class NodeAddress(
                        address: InetAddress,
                        connected: String
                      ) extends NetworkResult