package org.bitcoins.commons.jsonmodels

import org.bitcoins.commons.jsonmodels.SerializedTransaction._
import org.bitcoins.commons.serializers.JsonSerializers._
import org.bitcoins.core.crypto.ExtPublicKey
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.psbt._
import org.bitcoins.core.script.constant.ScriptToken
import org.bitcoins.core.script.crypto.HashType
import play.api.libs.json._
import scodec.bits.ByteVector

case class SerializedPSBT(
    global: SerializedPSBTGlobalMap,
    inputs: Vector[SerializedPSBTInputMap],
    outputs: Vector[SerializedPSBTOutputMap]) {
  val toJson: JsValue = Json.toJson(this)
}

case class SerializedPSBTGlobalMap(
    tx: SerializedTransaction,
    version: UInt32,
    xpubs: Option[Vector[ExtPublicKey]],
    unknowns: Vector[GlobalPSBTRecord.Unknown])

case class SerializedPSBTInputMap(
    nonWitnessUtxo: Option[SerializedTransaction],
    witnessUtxo: Option[SerializedTransactionOutput],
    signatures: Option[Vector[PartialSignature]],
    sigHashType: Option[HashType],
    redeemScript: Option[Vector[ScriptToken]],
    witScript: Option[Vector[ScriptToken]],
    bip32Paths: Option[Vector[InputPSBTRecord.BIP32DerivationPath]],
    finalizedScriptSig: Option[Vector[ScriptToken]],
    finalizedScriptWitness: Option[SerializedTransactionWitness],
    proofOfReservesCommitment: Option[ByteVector],
    unknowns: Vector[InputPSBTRecord.Unknown])

case class SerializedPSBTOutputMap(
    redeemScript: Option[Vector[ScriptToken]],
    witScript: Option[Vector[ScriptToken]],
    bip32Paths: Option[Vector[OutputPSBTRecord.BIP32DerivationPath]],
    unknowns: Vector[OutputPSBTRecord.Unknown])

object SerializedPSBT {

  def decodeGlobalMap(global: GlobalPSBTMap): SerializedPSBTGlobalMap = {
    val decodedTx = decodeRawTransaction(global.unsignedTransaction.transaction)
    val version = global.version.version
    val xpubs = global.extendedPublicKeys.map(_.xpub)
    val xpubsOpt = if (xpubs.nonEmpty) Some(xpubs) else None
    val unknownRecords = global.getRecords(PSBTGlobalKeyId.UnknownKeyId)

    SerializedPSBTGlobalMap(decodedTx, version, xpubsOpt, unknownRecords)
  }

  def decodeInputMap(
      input: InputPSBTMap,
      index: Int): SerializedPSBTInputMap = {
    val prevTxOpt = input.nonWitnessOrUnknownUTXOOpt.map(_.transactionSpent)
    val nonWitnessUtxo = prevTxOpt.map(decodeRawTransaction)
    val witnessUtxo = input.witnessUTXOOpt.map(rec =>
      decodeTransactionOutput(rec.witnessUTXO, index))

    val sigs = input.partialSignatures
    val sigsOpt = if (sigs.nonEmpty) Some(sigs) else None
    val hashType = input.sigHashTypeOpt.map(_.hashType)
    val redeemScript = input.redeemScriptOpt.map(_.redeemScript.asm.toVector)
    val witScript = input.witnessScriptOpt.map(_.witnessScript.asm.toVector)
    val bip32Paths = input.BIP32DerivationPaths
    val bip32PathsOpt = if (bip32Paths.nonEmpty) Some(bip32Paths) else None

    val finalizedScriptSig =
      input.finalizedScriptSigOpt.map(_.scriptSig.asm.toVector)
    val finalizedWitScript = input.finalizedScriptWitnessOpt.flatMap(rec =>
      decodeRawTransactionWitness(rec.scriptWitness))

    val porCommit = input.proofOfReservesCommitmentOpt.map(_.porCommitment)

    val unknowns = input.getRecords(PSBTInputKeyId.UnknownKeyId)

    SerializedPSBTInputMap(nonWitnessUtxo,
                           witnessUtxo,
                           sigsOpt,
                           hashType,
                           redeemScript,
                           witScript,
                           bip32PathsOpt,
                           finalizedScriptSig,
                           finalizedWitScript,
                           porCommit,
                           unknowns)
  }

  def decodeOutputMap(output: OutputPSBTMap): SerializedPSBTOutputMap = {
    val redeemScript = output.redeemScriptOpt.map(_.redeemScript.asm.toVector)
    val witScript = output.witnessScriptOpt.map(_.witnessScript.asm.toVector)
    val bip32Paths = output.BIP32DerivationPaths
    val bip32PathsOpt = if (bip32Paths.nonEmpty) Some(bip32Paths) else None
    val unknowns = output.getRecords(PSBTOutputKeyId.UnknownKeyId)

    SerializedPSBTOutputMap(redeemScript, witScript, bip32PathsOpt, unknowns)
  }

  def decodePSBT(psbt: PSBT): SerializedPSBT = {
    val global = decodeGlobalMap(psbt.globalMap)
    val inputs = psbt.inputMaps.zipWithIndex.map { case (input, index) =>
      decodeInputMap(input, index)
    }
    val outputs = psbt.outputMaps.map(decodeOutputMap)

    SerializedPSBT(global, inputs, outputs)
  }
}
