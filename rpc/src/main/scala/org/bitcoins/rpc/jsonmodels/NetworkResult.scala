package org.bitcoins.rpc.jsonmodels

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.number.{ Int32, UInt32 }

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