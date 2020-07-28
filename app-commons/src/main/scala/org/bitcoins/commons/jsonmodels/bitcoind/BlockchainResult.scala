package org.bitcoins.commons.jsonmodels.bitcoind

import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.gcs.GolombFilter
import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.core.wallet.fee.BitcoinFeeUnit
import org.bitcoins.crypto.DoubleSha256DigestBE

sealed abstract class BlockchainResult

case class GetBlockResult(
    hash: DoubleSha256DigestBE,
    confirmations: Int,
    strippedsize: Int,
    size: Int,
    weight: Int,
    height: Int,
    version: Int,
    versionHex: Int32,
    merkleroot: DoubleSha256DigestBE,
    tx: Vector[DoubleSha256DigestBE],
    time: UInt32,
    mediantime: UInt32,
    nonce: UInt32,
    bits: UInt32,
    difficulty: BigDecimal,
    chainwork: String,
    previousblockhash: Option[DoubleSha256DigestBE],
    nextblockhash: Option[DoubleSha256DigestBE])
    extends BlockchainResult

case class GetBlockWithTransactionsResult(
    hash: DoubleSha256DigestBE,
    confirmations: Int,
    strippedsize: Int,
    size: Int,
    weight: Int,
    height: Int,
    version: Int,
    versionHex: Int32,
    merkleroot: DoubleSha256DigestBE,
    tx: Vector[RpcTransaction],
    time: UInt32,
    mediantime: UInt32,
    nonce: UInt32,
    bits: UInt32,
    difficulty: BigDecimal,
    chainwork: String,
    previousblockhash: Option[DoubleSha256DigestBE],
    nextblockhash: Option[DoubleSha256DigestBE])
    extends BlockchainResult

sealed trait GetBlockChainInfoResult extends BlockchainResult {
  def chain: NetworkParameters
  def blocks: Int
  def headers: Int
  def bestblockhash: DoubleSha256DigestBE
  def difficulty: BigDecimal
  def mediantime: Int
  def verificationprogress: BigDecimal
  def initialblockdownload: Boolean
  def chainwork: String // How should this be handled?
  def size_on_disk: Long
  def pruned: Boolean
  def pruneheight: Option[Int]
  def warnings: String
}

case class GetBlockChainInfoResultPreV19(
    chain: NetworkParameters,
    blocks: Int,
    headers: Int,
    bestblockhash: DoubleSha256DigestBE,
    difficulty: BigDecimal,
    mediantime: Int,
    verificationprogress: BigDecimal,
    initialblockdownload: Boolean,
    chainwork: String, // How should this be handled?
    size_on_disk: Long,
    pruned: Boolean,
    pruneheight: Option[Int],
    softforks: Vector[SoftforkPreV19],
    bip9_softforks: Map[String, Bip9SoftforkPreV19],
    warnings: String)
    extends GetBlockChainInfoResult

case class GetBlockChainInfoResultPostV19(
    chain: NetworkParameters,
    blocks: Int,
    headers: Int,
    bestblockhash: DoubleSha256DigestBE,
    difficulty: BigDecimal,
    mediantime: Int,
    verificationprogress: BigDecimal,
    initialblockdownload: Boolean,
    chainwork: String, // How should this be handled?
    size_on_disk: Long,
    pruned: Boolean,
    pruneheight: Option[Int],
    softforks: Map[String, SoftforkPostV19],
    warnings: String)
    extends GetBlockChainInfoResult

case class SoftforkPreV19(
    id: String,
    version: Int,
    enforce: Option[Map[String, SoftforkProgressPreV19]],
    reject: SoftforkProgressPreV19)
    extends BlockchainResult

case class SoftforkProgressPreV19(
    status: Option[Boolean],
    found: Option[Int],
    required: Option[Int],
    window: Option[Int])
    extends BlockchainResult

case class Bip9SoftforkPreV19(
    status: String,
    bit: Option[Int],
    startTime: Int,
    timeout: BigInt,
    since: Int)
    extends BlockchainResult

sealed trait SoftforkPostV19 extends BlockchainResult

case class BuriedSoftforkPostV19(active: Boolean, height: Long)
    extends SoftforkPostV19

case class Bip9SoftforkPostV19(active: Boolean, bip9: Bip9SoftforkDetails)
    extends SoftforkPostV19

case class Bip9SoftforkDetails(
    status: String,
    bit: Option[Int],
    start_time: Int,
    timeout: BigInt,
    since: Int)
    extends BlockchainResult

case class GetBlockHeaderResult(
    hash: DoubleSha256DigestBE,
    confirmations: Int,
    height: Int,
    version: Int,
    versionHex: Int32,
    merkleroot: DoubleSha256DigestBE,
    time: UInt32,
    mediantime: UInt32,
    nonce: UInt32,
    bits: UInt32,
    difficulty: BigDecimal,
    chainwork: String,
    previousblockhash: Option[DoubleSha256DigestBE],
    nextblockhash: Option[DoubleSha256DigestBE])
    extends BlockchainResult {

  def blockHeader: BlockHeader = {

    //prevblockhash is only empty if we have the genesis block
    //we assume the prevhash of the gensis block is the empty hash
    val prevHash = {
      if (height == 0 && previousblockhash.isEmpty) {
        DoubleSha256DigestBE.empty
      } else {
        previousblockhash.get
      }
    }
    BlockHeader(version = Int32(version),
                previousBlockHash = prevHash.flip,
                merkleRootHash = merkleroot.flip,
                time = time,
                nBits = bits,
                nonce = nonce)
  }
}

case class ChainTip(
    height: Int,
    hash: DoubleSha256DigestBE,
    branchlen: Int,
    status: String)
    extends BlockchainResult

case class GetChainTxStatsResult(
    time: UInt32,
    txcount: Int,
    window_block_count: Int,
    window_final_block_height: Option[Int],
    window_tx_count: Option[Int],
    window_interval: Option[UInt32],
    txrate: Option[BigDecimal])
    extends BlockchainResult

sealed trait GetMemPoolResult extends BlockchainResult {
  def size: Int
  def fee: Option[Bitcoins]
  def modifiedfee: Option[Bitcoins]
  def time: UInt32
  def height: Int
  def descendantcount: Int
  def descendantsize: Int
  def descendantfees: Option[Bitcoins]
  def ancestorcount: Int
  def ancestorsize: Int
  def ancestorfees: Option[Bitcoins]
  def wtxid: DoubleSha256DigestBE
  def fees: FeeInfo
  def depends: Vector[DoubleSha256DigestBE]
}

case class GetMemPoolResultPreV19(
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
    wtxid: DoubleSha256DigestBE,
    fees: FeeInfo,
    depends: Vector[DoubleSha256DigestBE])
    extends GetMemPoolResult

case class GetMemPoolResultPostV19(
    vsize: Int,
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
    wtxid: DoubleSha256DigestBE,
    fees: FeeInfo,
    depends: Vector[DoubleSha256DigestBE])
    extends GetMemPoolResult {
  override def size: Int = vsize
}

case class FeeInfo(
    base: BitcoinFeeUnit,
    modified: BitcoinFeeUnit,
    ancestor: BitcoinFeeUnit,
    descendant: BitcoinFeeUnit
)

sealed trait GetMemPoolEntryResult extends BlockchainResult {
  def size: Int
  def fee: Bitcoins
  def modifiedfee: Bitcoins
  def time: UInt32
  def height: Int
  def descendantcount: Int
  def descendantsize: Int
  def descendantfees: BitcoinFeeUnit
  def ancestorcount: Int
  def ancestorsize: Int
  def ancestorfees: BitcoinFeeUnit
  def wtxid: DoubleSha256DigestBE
  def fees: FeeInfo
  def depends: Option[Vector[DoubleSha256DigestBE]]
}

case class GetMemPoolEntryResultPreV19(
    size: Int,
    fee: Bitcoins,
    modifiedfee: Bitcoins,
    time: UInt32,
    height: Int,
    descendantcount: Int,
    descendantsize: Int,
    descendantfees: BitcoinFeeUnit,
    ancestorcount: Int,
    ancestorsize: Int,
    ancestorfees: BitcoinFeeUnit,
    wtxid: DoubleSha256DigestBE,
    fees: FeeInfo,
    depends: Option[Vector[DoubleSha256DigestBE]])
    extends GetMemPoolEntryResult

case class GetMemPoolEntryResultPostV19(
    vsize: Int,
    fee: Bitcoins,
    weight: Int,
    modifiedfee: Bitcoins,
    time: UInt32,
    height: Int,
    descendantcount: Int,
    descendantsize: Int,
    descendantfees: BitcoinFeeUnit,
    ancestorcount: Int,
    ancestorsize: Int,
    ancestorfees: BitcoinFeeUnit,
    wtxid: DoubleSha256DigestBE,
    fees: FeeInfo,
    depends: Option[Vector[DoubleSha256DigestBE]])
    extends GetMemPoolEntryResult {
  override def size: Int = vsize
}

case class GetMemPoolInfoResult(
    size: Int,
    bytes: Int,
    usage: Int,
    maxmempool: Int,
    mempoolminfee: BitcoinFeeUnit,
    minrelaytxfee: Bitcoins)
    extends BlockchainResult

case class GetTxOutResult(
    bestblock: DoubleSha256DigestBE,
    confirmations: Int,
    value: Bitcoins,
    scriptPubKey: RpcScriptPubKey,
    coinbase: Boolean)
    extends BlockchainResult

case class GetTxOutSetInfoResult(
    height: Int,
    bestblock: DoubleSha256DigestBE,
    transactions: Int,
    txouts: Int,
    bogosize: Int,
    hash_serialized_2: DoubleSha256DigestBE,
    disk_size: Int,
    total_amount: Bitcoins)
    extends BlockchainResult

case class GetBlockFilterResult(
    filter: GolombFilter,
    header: DoubleSha256DigestBE)
    extends BlockchainResult
