package org.bitcoins.dlc.wallet.models

import org.bitcoins.core.number._
import org.bitcoins.core.protocol.dlc.models.{
  DLCFundingInput,
  DLCFundingInputP2WPKHV0
}
import org.bitcoins.core.protocol.script.{ScriptPubKey, ScriptWitness}
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.crypto.Sha256Digest

case class DLCFundingInputDb(
    dlcId: Sha256Digest,
    isInitiator: Boolean,
    inputSerialId: UInt64,
    outPoint: TransactionOutPoint,
    output: TransactionOutput,
    nSequence: UInt32,
    maxWitnessLength: Long,
    redeemScriptOpt: Option[ScriptPubKey],
    witnessScriptOpt: Option[ScriptWitness]) {

  lazy val toOutputReference: OutputReference =
    OutputReference(outPoint, output)

  def toFundingInput(prevTx: Transaction): DLCFundingInput = {
    require(
      prevTx.txId == outPoint.txId,
      s"Provided previous transaction didn't match database outpoint outpoint=${outPoint.txIdBE.hex} prevTx.txId=${prevTx.txIdBE.hex}"
    )

    DLCFundingInputP2WPKHV0(inputSerialId, prevTx, outPoint.vout, nSequence)
  }
}
