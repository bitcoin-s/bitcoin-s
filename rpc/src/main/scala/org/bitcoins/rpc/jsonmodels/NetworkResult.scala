package org.bitcoins.rpc.jsonmodels

import java.net.URI

import org.bitcoins.core.crypto.{DoubleSha256Digest, ECPublicKey, Sha256Hash160Digest}
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.number.{Int32, UInt32, UInt64}
import org.bitcoins.core.protocol.{Address, BitcoinAddress}
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionInput}
import org.bitcoins.core.wallet.fee.BitcoinFeeUnit

sealed abstract class NetworkResult

case class ChainTip(
    height: Int,
    hash: DoubleSha256Digest,
    branchlen: Int,
    status: String)
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
    nextblockhash: Option[DoubleSha256Digest])
    extends NetworkResult

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
    hdmasterkeyid: Option[Sha256Hash160Digest])
    extends NetworkResult

case class NodeBan(
    address: URI,
    banned_until: UInt32,
    ban_created: UInt32,
    ban_reason: String)
    extends NetworkResult

case class Node(
    addednode: URI,
    connected: Option[Boolean],
    addresses: Option[Vector[NodeAddress]])
    extends NetworkResult
case class NodeAddress(address: URI, connected: String)
    extends NetworkResult

case class GetMemPoolEntryResult(
    size: Int,
    fee: Bitcoins,
    modifiedfee: Bitcoins,
    time: UInt32,
    height: Int,
    descendantcount: Int,
    descendantsize: Int,
    descendantfees: Bitcoins, // Should be BitcoinFeeUnit
    ancestorcount: Int,
    ancestorsize: Int,
    ancestorfees: Bitcoins, // Should be BitcoinFeeUnit
    depends: Option[Vector[DoubleSha256Digest]])
    extends NetworkResult

case class GetMemPoolInfoResult(
    size: Int,
    bytes: Int,
    usage: Int,
    maxmempool: Int,
    mempoolminfee: BitcoinFeeUnit,
    minrelaytxfee: Bitcoins)
    extends NetworkResult

case class GetTxOutSetInfoResult(
    height: Int,
    bestblock: DoubleSha256Digest,
    transactions: Int,
    txouts: Int,
    bogosize: Int,
    hash_serialized_2: DoubleSha256Digest,
    disk_size: Int,
    total_amount: Bitcoins)
    extends NetworkResult

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
    nextblockhash: Option[DoubleSha256Digest])
    extends NetworkResult

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
    nextblockhash: Option[DoubleSha256Digest])
    extends NetworkResult

case class RpcTransaction(
    txid: DoubleSha256Digest,
    hash: DoubleSha256Digest,
    version: Int,
    size: Int,
    vsize: Int,
    locktime: UInt32,
    vin: Vector[TransactionInput],
    vout: Vector[RpcTransactionOutput],
    hex: Option[Transaction])
    extends NetworkResult

case class RpcTransactionOutput(
    value: Bitcoins,
    n: Int,
    scriptPubKey: RpcScriptPubKey)
    extends NetworkResult

case class RpcScriptPubKey(
    asm: String,
    hex: String,
    reqSigs: Option[Int],
    scriptType: String,
    addresses: Option[Vector[BitcoinAddress]])
    extends NetworkResult

case class ListSinceBlockResult(
    transactions: Vector[Payment],
    lastblock: DoubleSha256Digest)
    extends NetworkResult

case class Payment(
    involvesWatchonly: Option[Boolean],
    account: Option[String],
    address: Option[BitcoinAddress],
    category: String,
    amount: Bitcoins,
    vout: Int,
    fee: Option[Bitcoins],
    confirmations: Int,
    generated: Option[Boolean],
    blockhash: Option[DoubleSha256Digest],
    blockindex: Option[Int],
    blocktime: Option[UInt32],
    txid: DoubleSha256Digest,
    walletconflicts: Vector[DoubleSha256Digest],
    time: UInt32,
    timereceived: UInt32,
    bip125_replaceable: String,
    comment: Option[String],
    to: Option[String])
    extends NetworkResult

case class ListTransactionsResult(
    account: Option[String],
    address: Option[BitcoinAddress],
    category: String,
    amount: Bitcoins,
    label: Option[String],
    vout: Option[Int],
    fee: Option[Bitcoins],
    confirmations: Option[Int],
    trusted: Option[Boolean],
    generated: Option[Boolean],
    blockhash: Option[DoubleSha256Digest],
    blockindex: Option[Int],
    blocktime: Option[UInt32],
    txid: Option[DoubleSha256Digest],
    walletconflicts: Option[Vector[DoubleSha256Digest]],
    time: UInt32,
    timereceived: Option[UInt32],
    comment: Option[String],
    to: Option[String],
    otheraccount: Option[String],
    bip125_replaceable: String,
    abandoned: Option[Boolean])
    extends NetworkResult

case class ReceivedAddress(
    involvesWatchonly: Option[Boolean],
    address: BitcoinAddress,
    account: String,
    amount: Bitcoins,
    confirmations: Int,
    label: String,
    txids: Vector[DoubleSha256Digest])
    extends NetworkResult

case class GetTransactionResult(
    amount: Bitcoins,
    fee: Option[Bitcoins],
    confirmations: Int,
    generated: Option[Boolean],
    blockhash: Option[DoubleSha256Digest],
    blockindex: Option[Int],
    blocktime: Option[UInt32],
    txid: DoubleSha256Digest,
    walletconflicts: Vector[DoubleSha256Digest],
    time: UInt32,
    timereceived: UInt32,
    bip125_replaceable: String,
    comment: Option[String],
    to: Option[String],
    details: Vector[TransactionDetails],
    hex: Transaction)
    extends NetworkResult

case class TransactionDetails(
    involvesWatchonly: Option[Boolean],
    account: Option[String],
    address: Option[BitcoinAddress],
    category: String,
    amount: Bitcoins,
    vout: Int,
    fee: Option[Bitcoins],
    abandoned: Option[Boolean])
    extends NetworkResult

case class UnspentOutput( // Naming?
    txid: DoubleSha256Digest,
    vout: Int,
    address: Option[BitcoinAddress],
    account: Option[String],
    scriptPubKey: Option[ScriptPubKey],
    reedemScript: Option[ScriptPubKey],
    amount: Bitcoins,
    confirmations: Int,
    spendable: Boolean,
    solvable: Boolean)
    extends NetworkResult

case class LockUnspentOutputParameter(txid: DoubleSha256Digest, vout: Int)
    extends NetworkResult

case class SignRawTransactionResult(hex: Transaction, complete: Boolean)
    extends NetworkResult

case class GetBlockChainInfoResult(
    chain: String,
    blocks: Int,
    headers: Int,
    bestblockhash: DoubleSha256Digest,
    difficulty: BigDecimal,
    mediantime: Int,
    verificationprogress: BigDecimal,
    initialblockdownload: Boolean,
    chainwork: String, // How should this be handled?
    size_on_disk: Int,
    pruned: Boolean,
    pruneheight: Option[Int],
    softforks: Vector[Softfork],
    bip9_softforks: Map[String, Bip9Softfork],
    warnings: String)
    extends NetworkResult

case class Softfork(
    id: String,
    version: Int,
    enforce: Option[Map[String, SoftforkProgress]],
    reject: SoftforkProgress)
    extends NetworkResult

case class SoftforkProgress(
    status: Option[Boolean],
    found: Option[Int],
    required: Option[Int],
    window: Option[Int])
    extends NetworkResult

case class Bip9Softfork(
    status: String,
    bit: Option[Int],
    startTime: Int,
    timeout: BigInt,
    since: Int)
    extends NetworkResult

case class EstimateSmartFeeResult(
    feerate: Option[BitcoinFeeUnit],
    errors: Option[Vector[String]],
    blocks: Int)
    extends NetworkResult

case class GetMemoryInfoResult(locked: MemoryManager) extends NetworkResult

case class MemoryManager(
    used: Int,
    free: Int,
    total: Int,
    locked: Int,
    chunks_used: Int,
    chunks_free: Int)
    extends NetworkResult

case class GetMemPoolResult(
    size: Int,
    fee: Option[Bitcoins],
    modifiedfee: Option[Bitcoins],
    time: UInt32,
    height: Int,
    descendantcount: Int,
    descendantsize: Int,
    descendantfees: Option[Bitcoins],
    ancestorcount: Int,
    ancestorsize: Int,
    ancestorfees: Option[Bitcoins],
    wtxid: DoubleSha256Digest,
    depends: Vector[DoubleSha256Digest])
    extends NetworkResult

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
