package org.bitcoins.esplora

import org.bitcoins.commons.serializers.JsonSerializers._
import org.bitcoins.core.currency._
import org.bitcoins.core.number._
import org.bitcoins.core.protocol._
import org.bitcoins.crypto._
import play.api.libs.json._

//import org.bitcoins.commons.serializers.JsonSerializers._

sealed abstract class EsploraJsonModel

object EsploraJsonModels {

  case class EsploraTransactionStatus(
      confirmed: Boolean,
      block_height: Option[Long],
      block_hash: Option[DoubleSha256DigestBE],
      block_time: Option[Long])
      extends EsploraJsonModel

  implicit val EsploraTransactionStatusReads: Reads[EsploraTransactionStatus] =
    Json.reads[EsploraTransactionStatus]

  case class EsploraTransaction(
      txid: DoubleSha256DigestBE,
      version: Int32,
      locktime: UInt32,
      size: Int,
      weight: Int,
      fee: Satoshis,
      status: EsploraTransactionStatus)
      extends EsploraJsonModel

  implicit val EsploraTransactionReads: Reads[EsploraTransaction] =
    Json.reads[EsploraTransaction]

  case class AddressChainStats(
      funded_txo_count: Int,
      funded_txo_sum: Satoshis,
      spent_txo_count: Int,
      spent_txo_sum: Satoshis)
      extends EsploraJsonModel

  implicit val addressChainStatsReads: Reads[AddressChainStats] =
    Json.reads[AddressChainStats]

  case class AddressStats(
      address: BitcoinAddress,
      chain_stats: AddressChainStats,
      mempool_stats: AddressChainStats)
      extends EsploraJsonModel {

    val totalReceived: CurrencyUnit =
      chain_stats.funded_txo_sum + mempool_stats.funded_txo_sum

    val totalSpent: CurrencyUnit =
      chain_stats.spent_txo_sum + mempool_stats.spent_txo_sum

    val balance: CurrencyUnit = totalReceived - totalSpent
  }

  implicit val addressStatsReads: Reads[AddressStats] =
    Json.reads[AddressStats]

  case class EsploraBlock(
      id: DoubleSha256DigestBE,
      height: Int,
      version: Int32,
      timestamp: UInt32,
      mediantime: UInt32,
      bits: UInt32,
      nonce: UInt32,
      merkle_root: DoubleSha256DigestBE,
      tx_count: Int,
      size: Int,
      weight: Int,
      previousblockhash: DoubleSha256DigestBE
  ) extends EsploraJsonModel

  implicit val EsploraBlockReads: Reads[EsploraBlock] =
    Json.reads[EsploraBlock]

}
