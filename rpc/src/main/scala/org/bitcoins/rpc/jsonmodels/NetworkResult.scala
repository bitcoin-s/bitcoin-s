package org.bitcoins.rpc.jsonmodels

import java.net.InetAddress

import org.bitcoins.core.crypto.{DoubleSha256Digest, ECPublicKey, Sha256Hash160Digest}
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.{Address, BitcoinAddress}
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.TransactionInput

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
  networks: Vector[Network],
  relayfee: Bitcoins,
  incrementalfee: Bitcoins,
  localadresses: Option[Vector[NetworkAddress]],
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
  difficulty: BigDecimal,
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
                                  addresses: Option[Vector[Address]],
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
                 addresses: Option[Vector[NodeAddress]]
               ) extends NetworkResult
case class NodeAddress(
                        address: InetAddress,
                        connected: String
                      ) extends NetworkResult

case class GetMemPoolEntryResult(
                                  size: Int,
                                  fee: Bitcoins,
                                  modifiedfee: Bitcoins,
                                  time: UInt32,
                                  height: Int,
                                  startingpriority: BigDecimal,
                                  currentpriority: BigDecimal,
                                  descendantcount: Int,
                                  descendantsize: Int,
                                  descendantfees: Int,
                                  ancestorcount: Int,
                                  ancestorsize: Int,
                                  ancestorfees: Int,
                                  depends: Option[Vector[DoubleSha256Digest]]
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

case class GetBlockResult(
                           hash: DoubleSha256Digest,
                           confirmations: Int,
                           strippedsize: Int,
                           size: Int,
                           weight: Int,
                           height: Int,
                           version: Int,
                           versionHex: Int32,
                           merkleroot: DoubleSha256Digest,
                           tx: Vector[DoubleSha256Digest],
                           time: UInt32,
                           mediantime: UInt32,
                           nonce: UInt32,
                           bits: UInt32,
                           difficulty: BigDecimal,
                           chainwork: String,
                           previousblockhash: Option[DoubleSha256Digest],
                           nextblockhash: Option[DoubleSha256Digest]
                         ) extends NetworkResult

case class GetBlockWithTransactionsResult(
                                           hash: DoubleSha256Digest,
                                           confirmations: Int,
                                           strippedsize: Int,
                                           size: Int,
                                           weight: Int,
                                           height: Int,
                                           version: Int,
                                           versionHex: Int32,
                                           merkleroot: DoubleSha256Digest,
                                           tx: Vector[RpcTransaction],
                                           time: UInt32,
                                           mediantime: UInt32,
                                           nonce: UInt32,
                                           bits: UInt32,
                                           difficulty: BigDecimal,
                                           chainwork: String,
                                           previousblockhash: Option[DoubleSha256Digest],
                                           nextblockhash: Option[DoubleSha256Digest]
                                         ) extends NetworkResult

case class RpcTransaction(
                           txid: DoubleSha256Digest,
                           hash: DoubleSha256Digest,
                           version: Int,
                           size: Int,
                           vsize: Int,
                           locktime: UInt32,
                           vin: Vector[TransactionInput],
                           vout: Vector[RpcTransactionOutput],
                           hex: Block
                         ) extends NetworkResult

case class RpcTransactionOutput(
                                 value: Bitcoins,
                                 n: Int,
                                 scriptPubKey: RpcScriptPubKey
                               ) extends NetworkResult

case class RpcScriptPubKey(
                            asm: String,
                            hex: String,
                            reqSigs: Int,
                            scriptType: String,
                            addresses: Vector[BitcoinAddress]
                          ) extends NetworkResult