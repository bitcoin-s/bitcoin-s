package org.bitcoins.rpc.jsonmodels

import java.io.File

import org.bitcoins.core.crypto.{DoubleSha256Digest, Sha256Hash160Digest}
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.{
  Address,
  BitcoinAddress,
  P2PKHAddress,
  P2SHAddress
}
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.wallet.fee.BitcoinFeeUnit

sealed abstract class WalletResult

case class MultiSigResult(address: BitcoinAddress, redeemScript: ScriptPubKey)
    extends WalletResult

case class BumpFeeResult(
    txid: DoubleSha256Digest,
    origfee: Bitcoins,
    fee: Bitcoins, // Should be BitcoinFeeUnit
    warnings: String)
    extends WalletResult

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
    extends WalletResult

case class TransactionDetails(
    involvesWatchonly: Option[Boolean],
    account: Option[String],
    address: Option[BitcoinAddress],
    category: String,
    amount: Bitcoins,
    vout: Int,
    fee: Option[Bitcoins],
    abandoned: Option[Boolean])
    extends WalletResult

case class GetWalletInfoResult(
    walletname: String,
    walletversion: Int,
    balance: Bitcoins,
    unconfirmed_balance: Bitcoins,
    immature_balance: Bitcoins,
    txcount: Int,
    keypoololdest: UInt32,
    keypoolsize: Int,
    keypoolsize_hd_internal: Int,
    paytxfee: BitcoinFeeUnit,
    hdmasterkeyid: Sha256Hash160Digest,
    unlocked_until: Option[Int])
    extends WalletResult

case class ImportMultiResult(success: Boolean, error: Option[ImportMultiError])
    extends WalletResult

case class ImportMultiError(code: Int, message: String) extends WalletResult

case class RpcAddress(
    address: BitcoinAddress,
    balance: Bitcoins,
    account: Option[String])
    extends WalletResult

case class RpcAccount(
    involvesWatchonly: Boolean,
    account: String,
    amount: Bitcoins,
    confirmations: Int)
    extends WalletResult

case class DumpWalletResult(filename: File)

case class RescanBlockChainResult(
    start_height: Int,
    stop_height: Int
) extends WalletResult

case class ReceivedAddress(
    involvesWatchonly: Option[Boolean],
    address: BitcoinAddress,
    account: String,
    amount: Bitcoins,
    confirmations: Int,
    label: String,
    txids: Vector[DoubleSha256Digest])
    extends WalletResult

case class ReceivedAccount(
    involvesWatchonly: Option[Boolean],
    account: String,
    amount: Bitcoins,
    confirmations: Int,
    lable: Option[String]
) extends WalletResult

case class ListSinceBlockResult(
    transactions: Vector[Payment],
    lastblock: DoubleSha256Digest)
    extends WalletResult

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
    extends WalletResult

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
    extends WalletResult

case class UnspentOutput(
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
    extends WalletResult
