package org.bitcoins.rpc.jsonmodels

import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.{ScriptPubKey, ScriptSignature}
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionInput}
import org.bitcoins.core.protocol.{BitcoinAddress, P2PKHAddress, P2SHAddress}

sealed abstract class RawTransactionResult

case class RpcTransaction(
    txid: DoubleSha256DigestBE,
    hash: DoubleSha256DigestBE,
    version: Int,
    size: Int,
    vsize: Int,
    locktime: UInt32,
    vin: Vector[TransactionInput],
    vout: Vector[RpcTransactionOutput],
    hex: Option[Transaction])
    extends RawTransactionResult

case class RpcTransactionOutput(
    value: Bitcoins,
    n: Int,
    scriptPubKey: RpcScriptPubKey)
    extends RawTransactionResult

/**
  * @see [[https://github.com/bitcoin/bitcoin/blob/fa6180188b8ab89af97860e6497716405a48bab6/src/script/standard.cpp#L27 standard.cpp]]
  *     from Bitcoin Core
  */
sealed abstract class RpcScriptType extends RawTransactionResult

object RpcScriptType {
  final case object NONSTANDARD extends RpcScriptType
  final case object PUBKEY extends RpcScriptType
  final case object PUBKEYHASH extends RpcScriptType
  final case object SCRIPTHASH extends RpcScriptType
  final case object MULTISIG extends RpcScriptType
  final case object NULLDATA extends RpcScriptType
  final case object WITNESS_V0_KEYHASH extends RpcScriptType
  final case object WITNESS_V0_SCRIPTHASH extends RpcScriptType
  final case object WITNESS_UNKNOWN extends RpcScriptType
}

case class RpcScriptPubKey(
    asm: String,
    hex: String,
    reqSigs: Option[Int],
    scriptType: RpcScriptType,
    addresses: Option[Vector[BitcoinAddress]])
    extends RawTransactionResult

case class DecodeScriptResult(
    asm: String,
    typeOfScript: Option[RpcScriptType],
    reqSigs: Option[Int],
    addresses: Option[Vector[P2PKHAddress]],
    p2sh: P2SHAddress)
    extends RawTransactionResult

case class FundRawTransactionResult(
    hex: Transaction,
    fee: Bitcoins,
    changepos: Int)
    extends RawTransactionResult

case class GetRawTransactionResult(
    in_active_blockchain: Option[Boolean],
    hex: Transaction,
    txid: DoubleSha256DigestBE,
    hash: DoubleSha256DigestBE,
    size: Int,
    vsize: Int,
    version: Int,
    locktime: UInt32,
    vin: Vector[GetRawTransactionVin],
    vout: Vector[RpcTransactionOutput],
    blockhash: Option[DoubleSha256DigestBE],
    confirmations: Option[Int],
    time: Option[UInt32],
    blocktime: Option[UInt32])
    extends RawTransactionResult

case class GetRawTransactionVin(
    txid: Option[DoubleSha256DigestBE],
    vout: Option[Int],
    scriptSig: Option[GetRawTransactionScriptSig],
    sequence: Option[BigDecimal],
    txinwitness: Option[Vector[String]] // Should be TransactionWitness?
) extends RawTransactionResult

case class GetRawTransactionScriptSig(asm: String, hex: ScriptSignature)
    extends RawTransactionResult

case class SignRawTransactionResult(
    hex: Transaction,
    complete: Boolean,
    errors: Option[Vector[SignRawTransactionError]])
    extends RawTransactionResult

case class SignRawTransactionError(
    txid: DoubleSha256DigestBE,
    vout: Int,
    scriptSig: ScriptPubKey,
    sequence: UInt32,
    error: String)
    extends RawTransactionResult
