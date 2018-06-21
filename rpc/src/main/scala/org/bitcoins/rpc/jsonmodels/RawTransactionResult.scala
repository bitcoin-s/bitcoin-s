package org.bitcoins.rpc.jsonmodels

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.{BitcoinAddress, P2PKHAddress, P2SHAddress}
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionInput}

sealed abstract class RawTransactionResult

case class RpcTransaction(
    txid: DoubleSha256Digest,
    hash: DoubleSha256Digest,
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

case class RpcScriptPubKey(
    asm: String,
    hex: String,
    reqSigs: Option[Int],
    scriptType: String,
    addresses: Option[Vector[BitcoinAddress]])
    extends RawTransactionResult

case class DecodeScriptResult(
    asm: String,
    typeOfScript: Option[String],
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
    txid: DoubleSha256Digest,
    hash: DoubleSha256Digest,
    size: Int,
    vsize: Int,
    version: Int,
    locktime: UInt32,
    vin: Vector[GetRawTransactionVin],
    vout: Vector[RpcTransactionOutput],
    blockhash: DoubleSha256Digest,
    confirmations: Int,
    time: UInt32,
    blocktime: UInt32
) extends RawTransactionResult

case class GetRawTransactionVin(
    txid: Option[DoubleSha256Digest],
    vout: Option[Int],
    scriptSig: Option[GetRawTransactionScriptSig],
    sequence: Option[BigDecimal],
    txinwitness: Option[Vector[String]] // Should be TransactionWitness?
) extends RawTransactionResult

case class GetRawTransactionScriptSig(asm: String, hex: ScriptPubKey)
    extends RawTransactionResult // is hex right???

case class SignRawTransactionResult(
    hex: Transaction,
    complete: Boolean,
    errors: Option[Vector[SignRawTransactionError]])
    extends RawTransactionResult

case class SignRawTransactionError(
    txid: DoubleSha256Digest,
    vout: Int,
    scriptSig: ScriptPubKey,
    sequence: UInt32,
    error: String
) extends RawTransactionResult
