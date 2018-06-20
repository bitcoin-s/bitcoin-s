package org.bitcoins.rpc.jsonmodels

import org.bitcoins.core.crypto.{ECPublicKey, Sha256Hash160Digest}
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.wallet.fee.BitcoinFeeUnit

sealed abstract class OtherResult

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
    script: Option[ScriptPubKey],
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
