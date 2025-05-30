package org.bitcoins.commons.jsonmodels.bitcoind

import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.LabelPurpose
import org.bitcoins.commons.rpc.BitcoindException
import org.bitcoins.core.crypto.{ExtPrivateKey, ExtPublicKey}
import org.bitcoins.core.currency.{Bitcoins, Satoshis}
import org.bitcoins.core.hd.BIP32Path
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.descriptor.Descriptor
import org.bitcoins.core.protocol.script.{ScriptPubKey, WitnessVersion}
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.script.ScriptType
import org.bitcoins.core.wallet.fee.BitcoinFeeUnit
import org.bitcoins.crypto.{
  DoubleSha256DigestBE,
  ECPublicKey,
  RipeMd160Digest,
  Sha256Hash160Digest
}

import java.time.ZonedDateTime

sealed abstract class WalletResult

trait MultiSigResult extends WalletResult {
  def address: BitcoinAddress
  def redeemScript: ScriptPubKey
}

case class MultiSigResultPostV20(
    address: BitcoinAddress,
    redeemScript: ScriptPubKey,
    descriptor: String,
    warnings: Option[String]
) //available in v23
    extends MultiSigResult

case class BumpFeeResult(
    txid: DoubleSha256DigestBE,
    origfee: Bitcoins,
    fee: Bitcoins, // TODO: Should be BitcoinFeeUnit
    errors: Vector[String]
) extends WalletResult

case class HDKeyDescriptor(desc: Descriptor, active: Boolean)
case class GetHDKeysResult(
    xpub: ExtPublicKey,
    has_private: Boolean,
    xprv: Option[ExtPrivateKey],
    descriptors: Vector[HDKeyDescriptor])
    extends WalletResult

sealed trait GetTransactionResult extends WalletResult {
  def amount: Bitcoins
  def fee: Option[Bitcoins]
  def confirmations: Int
  def generated: Option[Boolean]
  def blockhash: Option[DoubleSha256DigestBE]
  def blockindex: Option[Int]
  def blocktime: Option[UInt32]
  def txid: DoubleSha256DigestBE
  def walletconflicts: Vector[DoubleSha256DigestBE]
  def time: UInt32
  def timereceived: UInt32
  def bip125_replaceable: Option[String]
  def comment: Option[String]
  def to: Option[String]
  def details: Vector[TransactionDetails]
  def hex: Transaction
}

case class GetTransactionResultPreV28(
    amount: Bitcoins,
    fee: Option[Bitcoins],
    confirmations: Int,
    generated: Option[Boolean],
    blockhash: Option[DoubleSha256DigestBE],
    blockindex: Option[Int],
    blocktime: Option[UInt32],
    txid: DoubleSha256DigestBE,
    walletconflicts: Vector[DoubleSha256DigestBE],
    time: UInt32,
    timereceived: UInt32,
    bip125_replaceable: Option[String],
    comment: Option[String],
    to: Option[String],
    details: Vector[TransactionDetails],
    hex: Transaction
) extends GetTransactionResult

case class GetTransactionResultV28(
    amount: Bitcoins,
    fee: Option[Bitcoins],
    confirmations: Int,
    generated: Option[Boolean],
    blockhash: Option[DoubleSha256DigestBE],
    blockindex: Option[Int],
    blocktime: Option[UInt32],
    txid: DoubleSha256DigestBE,
    walletconflicts: Vector[DoubleSha256DigestBE],
    time: UInt32,
    timereceived: UInt32,
    bip125_replaceable: Option[String],
    comment: Option[String],
    to: Option[String],
    details: Vector[TransactionDetails],
    hex: Transaction,
    mempoolconflicts: Vector[Transaction]
) extends GetTransactionResult

case class SetWalletFlagResult(
    flag_name: String,
    flag_state: Boolean,
    warnings: Option[String]
) extends WalletResult

case class GetBalancesResult(mine: BalanceInfo, watchonly: Option[BalanceInfo])
    extends WalletResult

case class BalanceInfo(
    trusted: Bitcoins,
    untrusted_pending: Bitcoins,
    immature: Bitcoins
)

case class TransactionDetails(
    involvesWatchonly: Option[Boolean],
    account: Option[String],
    address: Option[BitcoinAddress],
    category: String,
    amount: Bitcoins,
    vout: Int,
    fee: Option[Bitcoins],
    abandoned: Option[Boolean]
) extends WalletResult

sealed trait GetWalletInfoResult extends WalletResult {
  def walletname: String
  def walletversion: Int
  def balance: Bitcoins
  def unconfirmed_balance: Bitcoins
  def immature_balance: Bitcoins
  def txcount: Int
  def keypoololdest: Option[UInt32]
  def keypoolsize: Int
  def keypoolsize_hd_internal: Int
  def paytxfee: BitcoinFeeUnit
  def hdmasterkeyid: Option[Sha256Hash160Digest]
  def unlocked_until: Option[Int]

}

case class GetWalletInfoResultPostV22(
    walletname: String,
    walletversion: Int,
    balance: Bitcoins,
    unconfirmed_balance: Bitcoins,
    immature_balance: Bitcoins,
    txcount: Int,
    keypoololdest: Option[UInt32],
    keypoolsize: Int,
    keypoolsize_hd_internal: Int,
    paytxfee: BitcoinFeeUnit,
    hdmasterkeyid: Option[Sha256Hash160Digest],
    unlocked_until: Option[Int],
    private_keys_enabled: Boolean,
    descriptors: Boolean
) extends GetWalletInfoResult

case class ImportMultiResult(success: Boolean, error: Option[ImportMultiError])
    extends WalletResult

case class ImportMultiError(code: Int, message: String) extends WalletResult

case class RpcAddress(
    address: BitcoinAddress,
    balance: Bitcoins,
    account: Option[String]
) extends WalletResult

case class RpcAccount(
    involvesWatchonly: Boolean,
    account: String,
    amount: Bitcoins,
    confirmations: Int
) extends WalletResult
case class LoadWalletResult(name: String, warning: Option[String])
    extends WalletResult

case class RescanBlockChainResult(start_height: Int, stop_height: Int)
    extends WalletResult

case class ReceivedAddress(
    involvesWatchonly: Option[Boolean],
    address: BitcoinAddress,
    account: Option[String],
    amount: Bitcoins,
    confirmations: Int,
    label: String,
    txids: Vector[DoubleSha256DigestBE]
) extends WalletResult

case class ReceivedAccount(
    involvesWatchonly: Option[Boolean],
    account: String,
    amount: Bitcoins,
    confirmations: Int,
    lable: Option[String]
) extends WalletResult

case class ReceivedLabel(
    involvesWatchonly: Option[Boolean],
    amount: Bitcoins,
    confirmations: Int,
    label: String
) extends WalletResult

case class ListSinceBlockResult(
    transactions: Vector[Payment],
    lastblock: DoubleSha256DigestBE
) extends WalletResult

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
    blockhash: Option[DoubleSha256DigestBE],
    blockindex: Option[Int],
    blocktime: Option[UInt32],
    txid: DoubleSha256DigestBE,
    walletconflicts: Vector[DoubleSha256DigestBE],
    time: UInt32,
    timereceived: UInt32,
    bip125_replaceable: String,
    comment: Option[String],
    to: Option[String]
) extends WalletResult

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
    blockhash: Option[DoubleSha256DigestBE],
    blockheight: Option[Int],
    blockindex: Option[Int],
    blocktime: Option[UInt32],
    txid: Option[DoubleSha256DigestBE],
    walletconflicts: Option[Vector[DoubleSha256DigestBE]],
    time: UInt32,
    timereceived: Option[UInt32],
    comment: Option[String],
    to: Option[String],
    otheraccount: Option[String],
    bip125_replaceable: String,
    abandoned: Option[Boolean]
) extends WalletResult

case class UnspentOutput(
    txid: DoubleSha256DigestBE,
    vout: Int,
    address: Option[BitcoinAddress],
    account: Option[String],
    scriptPubKey: Option[ScriptPubKey],
    reedemScript: Option[ScriptPubKey],
    amount: Bitcoins,
    confirmations: Int,
    spendable: Boolean,
    solvable: Boolean,
    reused: Option[Boolean]
) extends WalletResult

sealed trait AddressInfoResult extends WalletResult {
  def address: BitcoinAddress
  def scriptPubKey: ScriptPubKey
  def ismine: Boolean
  def iswatchonly: Boolean
  def isscript: Boolean
  def iswitness: Boolean
  def iscompressed: Option[Boolean]
  def witness_version: Option[WitnessVersion]
  def witness_program: Option[String] // todo what's the correct type here?
  def script: Option[ScriptType]
  def hex: Option[ScriptPubKey]
  def pubkeys: Option[Vector[ECPublicKey]]
  def sigsrequired: Option[Int]
  def pubkey: Option[ECPublicKey]
  def embedded: Option[EmbeddedResult]
  def label: String
  def timestamp: Option[ZonedDateTime]
  def hdkeypath: Option[BIP32Path]
  def hdseedid: Option[RipeMd160Digest]
}

case class AddressInfoResultPostV21(
    address: BitcoinAddress,
    scriptPubKey: ScriptPubKey,
    isProps: AddressInfoResultPostV21.AddressInfoIsProps,
    desc: String,
    witness_version: Option[WitnessVersion],
    witness_program: Option[String],
    script: Option[ScriptType],
    hex: Option[ScriptPubKey],
    pubkeys: Option[Vector[ECPublicKey]],
    sigsrequired: Option[Int],
    pubkey: Option[ECPublicKey],
    embedded: Option[EmbeddedResult],
    ischange: Boolean,
    timestamp: Option[ZonedDateTime],
    hdkeypath: Option[BIP32Path],
    hdseedid: Option[RipeMd160Digest],
    hdmasterfingerprint: Option[String],
    labels: Vector[String]
) extends AddressInfoResult {
  override val label: String = labels.mkString(", ")
  override val ismine: Boolean = isProps.ismine
  val solvable: Boolean = isProps.solvable
  override val iswatchonly: Boolean = isProps.iswatchonly
  override val isscript: Boolean = isProps.isscript
  override val iswitness: Boolean = isProps.iswitness
  override val iscompressed: Option[Boolean] = isProps.iscompressed
}

object AddressInfoResultPostV21 {

  case class AddressInfoIsProps(
      ismine: Boolean,
      solvable: Boolean,
      iswatchonly: Boolean,
      isscript: Boolean,
      iswitness: Boolean,
      iscompressed: Option[Boolean]
  )

  case class AddressInfoResultPostV21WithoutIsProps(
      address: BitcoinAddress,
      scriptPubKey: ScriptPubKey,
      desc: String,
      witness_version: Option[WitnessVersion],
      witness_program: Option[String],
      script: Option[ScriptType],
      hex: Option[ScriptPubKey],
      pubkeys: Option[Vector[ECPublicKey]],
      sigsrequired: Option[Int],
      pubkey: Option[ECPublicKey],
      embedded: Option[EmbeddedResult],
      ischange: Boolean,
      timestamp: Option[ZonedDateTime],
      hdkeypath: Option[BIP32Path],
      hdseedid: Option[RipeMd160Digest],
      hdmasterfingerprint: Option[String],
      labels: Vector[String]
  )

  def apply(
      info: AddressInfoResultPostV21WithoutIsProps,
      isProps: AddressInfoIsProps
  ): AddressInfoResultPostV21 = {
    AddressInfoResultPostV21(
      address = info.address,
      scriptPubKey = info.scriptPubKey,
      isProps = isProps,
      desc = info.desc,
      witness_version = info.witness_version,
      witness_program = info.witness_program,
      script = info.script,
      hex = info.hex,
      pubkeys = info.pubkeys,
      sigsrequired = info.sigsrequired,
      pubkey = info.pubkey,
      embedded = info.embedded,
      ischange = info.ischange,
      timestamp = info.timestamp,
      hdkeypath = info.hdkeypath,
      hdseedid = info.hdseedid,
      hdmasterfingerprint = info.hdmasterfingerprint,
      labels = info.labels
    )
  }
}

case class ListDescriptorsResult(
    wallet_name: String,
    descriptors: Vector[DescriptorsResult]
) extends WalletResult

case class DescriptorsResult(
    desc: Descriptor,
    timestamp: Long,
    active: Boolean,
    internal: Option[Boolean],
    range: Option[Vector[Int]],
    next: Option[Int]
) extends WalletResult {

  override def toString(): String = {
    s"${getClass.getSimpleName}(desc=$desc,timestamp=$timestamp,active=$active,internal=$internal,range=$range,next=$next)"
  }
}

case class EmbeddedResult(
    isscript: Boolean,
    iswitness: Boolean,
    witness_version: WitnessVersion,
    witness_program: Option[String],
    pubkey: ECPublicKey,
    address: BitcoinAddress,
    scriptPubKey: ScriptPubKey
) extends WalletResult

case class LabelResult(name: String, purpose: LabelPurpose) extends WalletResult

final case class ListWalletDirResult(
    wallets: Vector[ArrayOfWalletsInput]
) extends WalletResult

final case class ArrayOfWalletsInput(
    name: String
) extends WalletResult

final case class CreateWalletResult(
    name: String,
    warning: Option[String]
) extends WalletResult

case class ImportDescriptorResult(
    success: Boolean,
    warnings: Option[Vector[String]],
    error: Option[BitcoindException]
) extends WalletResult

case class PrioritisedTransaction(fee_delta: Satoshis, in_mempool: Boolean)

case class CreateWalletDescriptorResult(descs: Vector[Descriptor])
