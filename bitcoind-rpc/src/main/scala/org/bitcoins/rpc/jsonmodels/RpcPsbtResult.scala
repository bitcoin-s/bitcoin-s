package org.bitcoins.rpc.jsonmodels

import org.bitcoins.core.crypto.{ECDigitalSignature, ECPublicKey}
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.script.ScriptType
import org.bitcoins.core.script.crypto.HashType

sealed abstract class RpcPsbtResult

final case class WalletProcessPsbtResult(psbt: String, complete: Boolean)
    extends RpcPsbtResult

sealed abstract class FinalizePsbtResult extends RpcPsbtResult
final case class FinalizedPsbt(hex: Transaction) extends FinalizePsbtResult
final case class NonFinalizedPsbt(psbt: String) extends FinalizePsbtResult

final case class DecodePsbtResult(
    tx: RpcTransaction,
    unknown: Map[String, String],
    inputs: Vector[RpcPsbtInput],
    outputs: Vector[RpcPsbtOutput],
    fee: Option[Bitcoins])
    extends RpcPsbtResult

final case class RpcPsbtInput(
    nonWitnessUtxo: Option[RpcTransaction],
    witnessUtxo: Option[PsbtWitnessUtxoInput],
    partialSignatures: Option[Map[ECPublicKey, ECDigitalSignature]],
    sighash: Option[HashType],
    redeemScript: Option[RpcPsbtScript],
    witnessScript: Option[RpcPsbtScript],
    bip32Derivs: Option[Vector[PsbtBIP32Deriv]],
    finalScriptSig: Option[RpcPsbtScript],
    finalScriptwitness: Option[Vector[String]], // todo(torkelrogstad) needs example of what this looks like
    unknown: Option[Map[String, String]] // The unknown global fields
) extends RpcPsbtResult

final case class RpcPsbtScript(
    asm: String, // todo(torkelrogstad) split into Vector[ScriptToken]?
    hex: ScriptPubKey,
    scriptType: Option[ScriptType],
    address: Option[BitcoinAddress]
) extends RpcPsbtResult

final case class PsbtBIP32Deriv(
    pubkey: ECPublicKey,
    masterFingerprint: String, // todo(torkelrogstad)
    path: String
    // todo(torkelrogstad) there's more fields here
) extends RpcPsbtResult

final case class PsbtWitnessUtxoInput(
    amount: Bitcoins,
    scriptPubKey: RpcPsbtScript
) extends RpcPsbtResult

final case class RpcPsbtOutput(
    redeemScript: Option[RpcPsbtScript],
    witnessScript: Option[RpcPsbtScript],
    bip32Derivs: Option[Vector[PsbtBIP32Deriv]],
    unknown: Option[Map[String, String]]
) extends RpcPsbtResult

final case class WalletCreateFundedPsbtResult(
    psbt: String, // todo change me
    fee: Bitcoins,
    changepos: Int
) extends RpcPsbtResult

final case class AnalyzePsbtResult(
    inputs: Vector[AnalyzePsbtInput],
    estimated_vsize: Option[Double],
    estimated_feerate: Option[Double],
    fee: Option[Bitcoins],
    next: String
) extends RpcPsbtResult
final case class AnalyzePsbtInput(
    has_utxo: Boolean,
    is_final: Boolean,
    missing: Option[PsbtMissingData],
    next: Option[String]
) extends RpcPsbtResult
final case class PsbtMissingData(
    pubkeys: Option[Vector[ECPublicKey]],
    signatures: Option[Vector[ECDigitalSignature]],
    redeemscript: Option[RpcPsbtScript],
    witnessscript: Option[RpcPsbtScript]
) extends RpcPsbtResult
