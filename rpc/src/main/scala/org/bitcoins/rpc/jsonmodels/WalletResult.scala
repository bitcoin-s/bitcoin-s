package org.bitcoins.rpc.jsonmodels

import java.io.File

import org.bitcoins.core.crypto.{DoubleSha256Digest, Sha256Hash160Digest}
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.{Address, BitcoinAddress, P2PKHAddress, P2SHAddress}
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.wallet.fee.BitcoinFeeUnit

sealed abstract class WalletResult

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

case class BumpFeeResult(
    txid: DoubleSha256Digest,
    origfee: Bitcoins,
    fee: Bitcoins, // Should be BitcoinFeeUnit
    warnings: String)
    extends WalletResult

case class MultiSigResult(address: BitcoinAddress, redeemScript: ScriptPubKey)
    extends WalletResult

case class DecodeScriptResult(
    asm: String,
    typeOfScript: Option[String],
    reqSigs: Option[Int],
    addresses: Option[Vector[P2PKHAddress]],
    p2sh: P2SHAddress)
    extends WalletResult

case class FundRawTransactionResult(
    hex: Transaction,
    fee: Bitcoins,
    changepos: Int)
    extends WalletResult

case class RpcAccount(
    involvesWatchonly: Boolean,
    account: String,
    amount: Bitcoins,
    confirmations: Int)
    extends WalletResult

case class RpcAddress(
    address: BitcoinAddress,
    balance: Bitcoins,
    account: Option[String])
    extends WalletResult

case class ImportMultiResult(success: Boolean, error: Option[ImportMultiError])
    extends WalletResult

case class ImportMultiError(code: Int, message: String) extends WalletResult

case class DumpWalletResult(filename: File)

case class RescanBlockChainResult(
                                   start_height: Option[Int],
                                   stop_height: Option[Int]
                                 ) extends WalletResult