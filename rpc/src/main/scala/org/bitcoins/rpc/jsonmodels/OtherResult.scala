package org.bitcoins.rpc.jsonmodels

import org.bitcoins.core.crypto.{
  DoubleSha256Digest,
  ECPublicKey,
  Sha256Hash160Digest
}
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.wallet.fee.BitcoinFeeUnit
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
    height: Int)
    extends OtherResult

case class BlockTransaction(
    data: Transaction,
    txid: DoubleSha256Digest,
    hash: DoubleSha256Digest,
    depends: Vector[Int],
    fee: Satoshis,
    sigops: Int,
    weight: Int,
    required: Option[Boolean])
    extends OtherResult

case class GetMiningInfoResult(
    blocks: Int,
    currentblockweight: Int,
    currentblocktx: Int,
    difficulty: BigDecimal,
    networkhashps: BigDecimal,
    pooledtx: Int,
    chain: String,
    warnings: String)
    extends OtherResult

case class GetMemoryInfoResult(locked: MemoryManager) extends OtherResult

case class MemoryManager(
    used: Int,
    free: Int,
    total: Int,
    locked: Int,
    chunks_used: Int,
    chunks_free: Int)
    extends OtherResult

case class ValidateAddressResult(
    isvalid: Boolean,
    address: Option[BitcoinAddress],
    scriptPubKey: Option[ScriptPubKey],
    ismine: Option[Boolean],
    iswatchonly: Option[Boolean],
    isscript: Option[Boolean],
    script: Option[String],
    hex: Option[String],
    addresses: Option[Vector[BitcoinAddress]],
    sigrequired: Option[Int],
    pubkey: Option[ECPublicKey],
    iscompressed: Option[Boolean],
    account: Option[String],
    hdkeypath: Option[String],
    hdmasterkeyid: Option[Sha256Hash160Digest])
    extends OtherResult

case class EstimateSmartFeeResult(
    feerate: Option[BitcoinFeeUnit],
    errors: Option[Vector[String]],
    blocks: Int)
    extends OtherResult
