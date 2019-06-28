package org.bitcoins.rpc.jsonmodels

import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.core.wallet.fee.BitcoinFeeUnit
import org.bitcoins.core.config.NetworkParameters

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

case class GetBlockChainInfoResult(
    chain: NetworkParameters,
    blocks: Int,
    headers: Int,
    bestblockhash: DoubleSha256DigestBE,
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
    extends BlockchainResult

case class Softfork(
    id: String,
    version: Int,
    enforce: Option[Map[String, SoftforkProgress]],
    reject: SoftforkProgress)
    extends BlockchainResult

case class SoftforkProgress(
    status: Option[Boolean],
    found: Option[Int],
    required: Option[Int],
    window: Option[Int])
    extends BlockchainResult

case class Bip9Softfork(
    status: String,
    bit: Option[Int],
    startTime: Int,
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
    window_tx_count: Option[Int],
    window_interval: Option[UInt32],
    txrate: Option[BigDecimal])
    extends BlockchainResult

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
    wtxid: DoubleSha256DigestBE,
    depends: Vector[DoubleSha256DigestBE])
    extends BlockchainResult

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
    depends: Option[Vector[DoubleSha256DigestBE]])
    extends BlockchainResult

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
