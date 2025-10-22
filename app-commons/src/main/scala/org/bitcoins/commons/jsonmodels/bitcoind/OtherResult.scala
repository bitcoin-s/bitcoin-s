package org.bitcoins.commons.jsonmodels.bitcoind

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.script.descriptor.Descriptor
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.util.SeqWrapper
import org.bitcoins.core.wallet.fee.BitcoinFeeUnit
import org.bitcoins.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import play.api.libs.json.JsObject

sealed abstract class OtherResult

case class GetBlockTemplateResult(
    capabilities: Vector[String],
    version: Int,
    rules: Vector[String],
    vbavailable: Map[String, Int], // Is this Int or BigDecimal?
    vbrequired: Int,
    previousblockhash: DoubleSha256Digest,
    transactions: Vector[BlockTransaction],
    coinbaseaux: Map[String, String],
    coinbasevalue: Satoshis,
    longpollid: String, // What is this?
    coinbasetxn: Option[JsObject],
    target: String, // What should this be?
    mintime: UInt32,
    mutable: Vector[String],
    noncerange: String,
    sigoplimit: Int,
    sizelimit: Int,
    weightlimit: Int,
    curtime: UInt32,
    bits: String, // What should this be?
    height: Int
) extends OtherResult

case class BlockTransaction(
    data: Transaction,
    txid: DoubleSha256Digest,
    hash: DoubleSha256Digest,
    depends: Vector[Int],
    fee: Satoshis,
    sigops: Int,
    weight: Int,
    required: Option[Boolean]
) extends OtherResult

sealed abstract class GetMiningInfoResult extends OtherResult {
  def blocks: Int
  def currentblockweight: Option[Int]
  def currentblocktx: Option[Int]
  def difficulty: BigDecimal
  def networkhashps: BigDecimal
  def pooledtx: Int
  def chain: String
}

case class GetMiningInfoResultV28(
    blocks: Int,
    currentblockweight: Option[Int],
    currentblocktx: Option[Int],
    difficulty: BigDecimal,
    networkhashps: BigDecimal,
    pooledtx: Int,
    chain: String,
    warnings: Vector[String])
    extends GetMiningInfoResult

case class NextBlockMiningInfo(
    height: Int,
    bits: String,
    difficulty: BigDecimal,
    target: String)
case class GetMiningInfoResultV29(
    blocks: Int,
    currentblockweight: Option[Int],
    currentblocktx: Option[Int],
    difficulty: BigDecimal,
    networkhashps: BigDecimal,
    pooledtx: Int,
    chain: String,
    target: String,
    bits: String,
    next: NextBlockMiningInfo,
    warnings: Vector[String])
    extends GetMiningInfoResult

case class GetMiningInfoResultV30(
    blocks: Int,
    currentblockweight: Option[Int],
    currentblocktx: Option[Int],
    difficulty: BigDecimal,
    networkhashps: BigDecimal,
    pooledtx: Int,
    chain: String,
    target: String,
    bits: String,
    next: NextBlockMiningInfo,
    warnings: Vector[String],
    blockmintxfee: BigDecimal)
    extends GetMiningInfoResult

case class GetMemoryInfoResult(locked: MemoryManager) extends OtherResult

case class GenerateBlockResult(hash: DoubleSha256DigestBE) extends OtherResult

case class MemoryManager(
    used: Int,
    free: Int,
    total: Int,
    locked: Int,
    chunks_used: Int,
    chunks_free: Int
) extends OtherResult

/** @note
  *   This is defined as a trait and not just a raw case class (as is done in
  *   other RPC return values) in order to make it possible to deprecate fields.
  */
sealed trait ValidateAddressResult {
  def isvalid: Boolean
  def address: Option[BitcoinAddress]
  def scriptPubKey: Option[ScriptPubKey]
  def error_locations: Option[Vector[Int]]
  def isscript: Option[Boolean]
  def sigsrequired: Option[Int]
}

case class ValidateAddressResultImpl(
    isvalid: Boolean,
    address: Option[BitcoinAddress],
    scriptPubKey: Option[ScriptPubKey],
    error_locations: Option[Vector[Int]],
    isscript: Option[Boolean],
    sigsrequired: Option[Int]
) extends ValidateAddressResult

case class EstimateSmartFeeResult(
    feerate: Option[BitcoinFeeUnit],
    errors: Option[Vector[String]],
    blocks: Int
) extends OtherResult

case class TestMempoolAcceptResult(
    txid: DoubleSha256DigestBE,
    allowed: Boolean,
    rejectReason: Option[String]
)

/** sealed trait TestMempoolAcceptResult { def txid: DoubleSha256DigestBE def
  * allowed: Boolean def rejectReason: Option[String] }
  *
  * case class TestMempoolAcceptResultPreV22( txid: DoubleSha256DigestBE,
  * allowed: Boolean, rejectReason: Option[String] ) extends
  * TestMempoolAcceptResult
  */

case class FeeInfoTwo(
    base: BitcoinFeeUnit,
    effective_feerate: BigDecimal,
    effective_includes: Vector[DoubleSha256DigestBE]
)

case class TestMempoolAcceptResultPostV24(
    txid: DoubleSha256DigestBE,
    wtxid: DoubleSha256DigestBE,
    packageError: Option[String],
    allowed: Boolean,
    vsize: Option[Int],
    fees: Option[FeeInfoTwo],
    rejectReason: Option[String] // wtxid
)

final case class DeriveAddressesResult(addresses: Vector[BitcoinAddress])
    extends OtherResult
    with SeqWrapper[BitcoinAddress] {
  override protected val wrapped: Vector[BitcoinAddress] = addresses
}

final case class GetDescriptorInfoResult(
    descriptor: Descriptor,
    checksum: Option[String],
    isrange: Boolean,
    issolvable: Boolean,
    hasprivatekeys: Boolean
) extends OtherResult

final case class SubmitHeaderResult(header: BlockHeader) extends OtherResult

case class IndexInfoResult(synced: Boolean, best_block_height: Int)
    extends OtherResult
