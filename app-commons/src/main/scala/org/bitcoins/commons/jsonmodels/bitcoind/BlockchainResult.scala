package org.bitcoins.commons.jsonmodels.bitcoind

import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.{
  ScanBlocksAction,
  ScanBlocksOpt
}
import org.bitcoins.commons.serializers.JsonWriters.DescriptorWrites
import org.bitcoins.core.api.chain.db.{
  BlockHeaderDb,
  BlockHeaderDbHelper,
  CompactFilterDb,
  CompactFilterDbHelper
}
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.gcs.{FilterType, GolombFilter}
import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.script.descriptor.Descriptor
import org.bitcoins.core.protocol.transaction.TransactionOutPoint
import org.bitcoins.core.wallet.fee.BitcoinFeeUnit
import org.bitcoins.crypto.DoubleSha256DigestBE
import play.api.libs.json.{JsArray, JsNull, JsNumber, JsString, JsValue, Json}
import scodec.bits.ByteVector

import java.nio.file.Path

sealed abstract class BlockchainResult

case class DumpTxOutSetResult(
    coins_written: Int,
    base_hash: DoubleSha256DigestBE,
    base_height: Int,
    path: Path,
    txoutset_hash: DoubleSha256DigestBE,
    nchaintx: Long
) extends BlockchainResult

case class LoadTxOutSetResult(
    coins_loaded: Long,
    tip_hash: DoubleSha256DigestBE,
    base_height: Long,
    path: Path)
    extends BlockchainResult

case class ScanTxoutSetRequest(
    action: ScanBlocksAction,
    descs: Vector[Descriptor]) {
  def params: List[JsValue] = {
    action match {
      case ScanBlocksOpt.Start =>
        List(
          JsString(action.toString),
          Json.toJson(descs)
        )
      case ScanBlocksOpt.Status | ScanBlocksOpt.Abort =>
        List(JsString(action.toString))
    }

  }
}

case class ScanTxoutSetUTXO(
    txid: DoubleSha256DigestBE,
    vout: Int,
    scriptPubKey: ScriptPubKey,
    desc: Descriptor,
    amount: Bitcoins,
    coinbase: Boolean,
    height: Int,
    blockhash: DoubleSha256DigestBE,
    confirmations: Int)

case class ScanTxoutSetResult(
    success: Boolean,
    txouts: Int,
    height: Int,
    bestblock: DoubleSha256DigestBE,
    unspents: Vector[ScanTxoutSetUTXO],
    total_amount: Bitcoins
) extends BlockchainResult

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
    nextblockhash: Option[DoubleSha256DigestBE],
    target: Option[
      String
    ] // once v29 is minimal supported version, remove Option
) extends BlockchainResult

abstract trait GetBlockWithTransactionsResult extends BlockchainResult {
  def hash: DoubleSha256DigestBE
  def confirmations: Int
  def strippedsize: Int
  def size: Int
  def weight: Int
  def height: Int
  def version: Int
  def versionHex: Int32
  def merkleroot: DoubleSha256DigestBE
  def tx: Vector[RpcTransaction]
  def time: UInt32
  def mediantime: UInt32
  def nonce: UInt32
  def bits: UInt32
  def difficulty: BigDecimal
  def chainwork: String
  def previousblockhash: Option[DoubleSha256DigestBE]
  def nextblockhash: Option[DoubleSha256DigestBE]
}

case class GetBlockWithTransactionsResultV22(
    hash: DoubleSha256DigestBE,
    confirmations: Int,
    strippedsize: Int,
    size: Int,
    weight: Int,
    height: Int,
    version: Int,
    versionHex: Int32,
    merkleroot: DoubleSha256DigestBE,
    tx: Vector[RpcTransactionV22],
    time: UInt32,
    mediantime: UInt32,
    nonce: UInt32,
    bits: UInt32,
    difficulty: BigDecimal,
    chainwork: String,
    previousblockhash: Option[DoubleSha256DigestBE],
    nextblockhash: Option[DoubleSha256DigestBE]
) extends GetBlockWithTransactionsResult

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
}

case class GetBlockChainInfoResultPostV27(
    chain: NetworkParameters,
    blocks: Int,
    headers: Int,
    bestblockhash: DoubleSha256DigestBE,
    difficulty: BigDecimal,
    time: Int,
    mediantime: Int,
    verificationprogress: BigDecimal,
    initialblockdownload: Boolean,
    chainwork: String, // How should this be handled?
    size_on_disk: Long,
    pruned: Boolean,
    pruneheight: Option[Int],
    warnings: Vector[String],
    target: Option[
      String
    ] // once v29 is minimal supported version, remove Option
) extends GetBlockChainInfoResult

case class SoftforkPreV19(
    id: String,
    version: Int,
    enforce: Option[Map[String, SoftforkProgressPreV19]],
    reject: SoftforkProgressPreV19
) extends BlockchainResult

case class SoftforkProgressPreV19(
    status: Option[Boolean],
    found: Option[Int],
    required: Option[Int],
    window: Option[Int]
) extends BlockchainResult

case class Bip9SoftforkPreV19(
    status: String,
    bit: Option[Int],
    startTime: Int,
    timeout: BigInt,
    since: Int
) extends BlockchainResult

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
    since: Int
) extends BlockchainResult

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
    nextblockhash: Option[DoubleSha256DigestBE],
    target: Option[
      String
    ] // once v29 is minimal supported version, remove Option
) extends BlockchainResult {

  lazy val blockHeaderDb: BlockHeaderDb = {
    val bytes = ByteVector.fromValidHex(chainwork).dropWhile(_ == 0x00).toArray
    val chainWork = BigInt(1, bytes)
    BlockHeaderDbHelper.fromBlockHeader(height, chainWork, blockHeader)
  }

  def blockHeader: BlockHeader = {

    // prevblockhash is only empty if we have the genesis block
    // we assume the prevhash of the gensis block is the empty hash
    val prevHash = {
      if (height == 0 && previousblockhash.isEmpty) {
        DoubleSha256DigestBE.empty
      } else {
        previousblockhash.get
      }
    }
    BlockHeader(
      version = Int32(version),
      previousBlockHash = prevHash.flip,
      merkleRootHash = merkleroot.flip,
      time = time,
      nBits = bits,
      nonce = nonce
    )
  }
}

case class ChainTip(
    height: Int,
    hash: DoubleSha256DigestBE,
    branchlen: Int,
    status: String
) extends BlockchainResult

case class GetChainTxStatsResult(
    time: UInt32,
    txcount: Int,
    window_block_count: Int,
    window_final_block_height: Option[Int],
    window_tx_count: Option[Int],
    window_interval: Option[UInt32],
    txrate: Option[BigDecimal]
) extends BlockchainResult

sealed abstract class GetRawMempoolResult extends BlockchainResult {
  def txids: Vector[DoubleSha256DigestBE]
}

case class GetRawMempoolTxIds(txids: Vector[DoubleSha256DigestBE])
    extends GetRawMempoolResult

case class GetRawMempoolVerboseResult(
    vsize: Int,
    weight: Int,
    time: Long,
    height: Int,
    descendantcount: Int,
    descendantsize: Int,
    ancestorcount: Int,
    ancestorsize: Int,
    wtxid: DoubleSha256DigestBE,
    fees: FeeInfo,
    depends: Vector[DoubleSha256DigestBE],
    spentby: Vector[DoubleSha256DigestBE],
    `bip125-replaceable`: Boolean,
    unbroadcast: Boolean)

case class GetRawMempoolVerbose(
    map: Map[DoubleSha256DigestBE, GetRawMempoolVerboseResult])
    extends GetRawMempoolResult {
  override val txids: Vector[DoubleSha256DigestBE] = map.keys.toVector
}

sealed trait GetMemPoolResult extends BlockchainResult {
  def size: Int
  def time: UInt32
  def height: Int
  def descendantcount: Int
  def descendantsize: Int
  def ancestorcount: Int
  def ancestorsize: Int
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
    depends: Vector[DoubleSha256DigestBE]
) extends GetMemPoolResult

// v23 removes 'fee', 'modifiedfee', 'descendantfees', 'ancestorfees'
case class GetMemPoolResultPostV23(
    vsize: Int,
    time: UInt32,
    height: Int,
    descendantcount: Int,
    descendantsize: Int,
    ancestorcount: Int,
    ancestorsize: Int,
    wtxid: DoubleSha256DigestBE,
    fees: FeeInfo,
    depends: Vector[DoubleSha256DigestBE]
) extends GetMemPoolResult {
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
  def time: UInt32
  def height: Int
  def descendantcount: Int
  def descendantsize: Int
  def ancestorcount: Int
  def ancestorsize: Int
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
    depends: Option[Vector[DoubleSha256DigestBE]]
) extends GetMemPoolEntryResult

case class GetMemPoolEntryResultPostV23(
    vsize: Int,
    weight: Int,
    time: UInt32,
    height: Int,
    descendantcount: Int,
    descendantsize: Int,
    ancestorcount: Int,
    ancestorsize: Int,
    wtxid: DoubleSha256DigestBE,
    fees: FeeInfo,
    depends: Option[Vector[DoubleSha256DigestBE]]
) extends GetMemPoolEntryResult {
  override def size: Int = vsize
}

sealed abstract class GetMemPoolInfoResult extends BlockchainResult {
  def loaded: Boolean
  def size: Int
  def bytes: Int
  def usage: Int
  def maxmempool: Int
  def mempoolminfee: BitcoinFeeUnit
  def minrelaytxfee: Bitcoins
  def incrementalrelayfee: BigDecimal // BTC/kvb
  def unbroadcastcount: Int
  def fullrbf: Boolean
  def total_fee: Bitcoins
}
case class GetMemPoolInfoResultV29(
    loaded: Boolean,
    size: Int,
    bytes: Int,
    usage: Int,
    maxmempool: Int,
    mempoolminfee: BitcoinFeeUnit,
    minrelaytxfee: Bitcoins,
    incrementalrelayfee: BigDecimal, // BTC/kvb
    unbroadcastcount: Int,
    fullrbf: Boolean,
    total_fee: Bitcoins
) extends GetMemPoolInfoResult

case class GetMemPoolInfoResultV30(
    loaded: Boolean,
    size: Int,
    bytes: Int,
    usage: Int,
    maxmempool: Int,
    mempoolminfee: BitcoinFeeUnit,
    minrelaytxfee: Bitcoins,
    incrementalrelayfee: BigDecimal, // BTC/kvb
    unbroadcastcount: Int,
    fullrbf: Boolean,
    permitbaremultisig: Boolean,
    maxdatacarriersize: Int,
    total_fee: Bitcoins
) extends GetMemPoolInfoResult

sealed abstract trait GetTxOutResult extends BlockchainResult {
  def bestblock: DoubleSha256DigestBE
  def confirmations: Int
  def value: Bitcoins
  def scriptPubKey: RpcScriptPubKey
  def coinbase: Boolean
}

case class GetTxOutResultV22(
    bestblock: DoubleSha256DigestBE,
    confirmations: Int,
    value: Bitcoins,
    scriptPubKey: RpcScriptPubKeyPostV22,
    coinbase: Boolean
) extends GetTxOutResult

case class GetTxOutSetInfoResult(
    height: Int,
    bestblock: DoubleSha256DigestBE,
    transactions: Int,
    txouts: Int,
    bogosize: Int,
    hash_serialized_3: DoubleSha256DigestBE,
    disk_size: Int,
    total_amount: Bitcoins
) extends BlockchainResult

case class GetBlockFilterResult(
    filter: GolombFilter,
    header: DoubleSha256DigestBE
) extends BlockchainResult {

  def filterDb(
      height: Int,
      blockHashBE: DoubleSha256DigestBE
  ): CompactFilterDb = {
    CompactFilterDbHelper.fromGolombFilter(filter, blockHashBE, height)
  }
}

case class GetTxSpendingPrevOutResult(
    txid: DoubleSha256DigestBE,
    vout: Int,
    spendingtxid: Option[DoubleSha256DigestBE]
) {
  def outpoint: TransactionOutPoint = TransactionOutPoint(txid, UInt32(vout))
}

case class SimulateRawTransactionResult(balance_change: Bitcoins)

case class ScanObject(descriptor: Descriptor) {
  def toJson: JsString = JsString(descriptor.toString)
}

case class ScanBlocksRequest(
    action: ScanBlocksAction,
    scanObjects: Vector[ScanObject],
    startHeightOpt: Option[Int],
    stopHeightOpt: Option[Int],
    filterTypeOpt: Option[FilterType]) {
  def params: List[JsValue] = {
    List(
      JsString(action.toString),
      JsArray(scanObjects.map(_.toJson)), // scanobjects not supported for now?
      startHeightOpt.map(JsNumber(_)).getOrElse(JsNull),
      stopHeightOpt.map(JsNumber(_)).getOrElse(JsNull),
      filterTypeOpt.map(f => JsString(f.toString)).getOrElse(JsNull)
    )
  }
}

sealed trait ScanBlocksResult extends BlockchainResult
sealed trait StatusScanBlocksResult extends ScanBlocksResult

case class ScanBlocksStartResult(
    from_height: Int,
    to_height: Int,
    relevant_blocks: Vector[DoubleSha256DigestBE])
    extends ScanBlocksResult

case object NoScanInProgress extends StatusScanBlocksResult
case class ScanInProgress(progress: BigDecimal, current_height: Int)
    extends StatusScanBlocksResult

case class ScanBlocksAbortResult(aborted: Boolean) extends ScanBlocksResult

case class ChainState(
    blocks: Int,
    bestblockhash: DoubleSha256DigestBE,
    difficulty: BigDecimal,
    verificationprogress: BigDecimal,
    coins_db_cache_bytes: Long,
    coins_tip_cache_bytes: Long,
    validated: Boolean,
    target: Option[
      String
    ] // once v29 is minimal supported version, remove Option
)
case class ChainStateResult(headers: Int, chainstates: Vector[ChainState])

sealed trait DescriptorActivityType
object DescriptorActivityType {
  case object Receive extends DescriptorActivityType {
    override def toString: String = "receive"
  }
  case object Spend extends DescriptorActivityType {
    override def toString: String = "spend"
  }
}
sealed trait DescriptorActivity {
  def `type`: DescriptorActivityType
  def amount: Bitcoins
  def blockhash: Option[DoubleSha256DigestBE]
  def height: Option[Int]
}

object DescriptorActivity {
  case class SpendDescriptorActivity(
      amount: Bitcoins,
      blockhash: Option[DoubleSha256DigestBE],
      height: Option[Int],
      spend_txid: DoubleSha256DigestBE,
      spend_vin: Int,
      prevout_txid: DoubleSha256DigestBE,
      prevout_vout: Int,
      prevout_spk: RpcPsbtScript)
      extends DescriptorActivity {
    override val `type`: DescriptorActivityType = DescriptorActivityType.Spend
  }

  case class ReceiveDescriptorActivity(
      amount: Bitcoins,
      blockhash: Option[DoubleSha256DigestBE],
      height: Option[Int],
      txid: DoubleSha256DigestBE,
      vout: Int,
      output_spk: RpcPsbtScript)
      extends DescriptorActivity {
    override val `type`: DescriptorActivityType = DescriptorActivityType.Receive
  }
}

case class GetDescriptorActivityResult(activity: Vector[DescriptorActivity])

case class WaitForBlockResult(hash: DoubleSha256DigestBE, height: Int)
    extends BlockchainResult
