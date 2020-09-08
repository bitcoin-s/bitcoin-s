package org.bitcoins.dlc.wallet.models

import org.bitcoins.core.protocol.script.{ScriptPubKey, ScriptWitness}
import org.bitcoins.core.protocol.transaction.{
  OutputReference,
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.crypto.Sha256DigestBE

case class DLCFundingInputDb(
    paramHash: Sha256DigestBE,
    isInitiator: Boolean,
    outPoint: TransactionOutPoint,
    output: TransactionOutput,
    redeemScriptOpt: Option[ScriptPubKey],
    witnessScriptOpt: Option[ScriptWitness],
    sigs: Vector[PartialSignature]) {

  lazy val toOutputReference: OutputReference =
    OutputReference(outPoint, output)
}
