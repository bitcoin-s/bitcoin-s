package org.bitcoins.core.wallet

import org.bitcoins.core.crypto.{WitnessTxSigComponent, WitnessTxSigComponentP2SH, WitnessTxSigComponentRaw}
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.gen.TransactionGenerators
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script.{EmptyScriptSignature, ScriptWitness, WitnessScriptPubKey}
import org.bitcoins.core.protocol.transaction._

/**
  * Created by chris on 5/9/17.
  */
sealed trait WTxSigComponentHelper {
  /** Takes a signed [[ScriptWitness]] and an unsignedTx and adds the witness to the unsigned [[WitnessTransaction]] */
  def createSignedWTxComponent(witness: ScriptWitness, unsignedWTxComponent: WitnessTxSigComponent): (TransactionWitness,WitnessTxSigComponent) = {
    val updated = unsignedWTxComponent.transaction.witness.witnesses.updated(unsignedWTxComponent.inputIndex.toInt,witness)
    val signedTxWitness = TransactionWitness(updated)
    val unsignedSpendingTx = unsignedWTxComponent.transaction
    val signedSpendingTx = WitnessTransaction(unsignedSpendingTx.version,unsignedSpendingTx.inputs,unsignedSpendingTx.outputs,
      unsignedSpendingTx.lockTime, signedTxWitness)
    val signedWtxSigComponent = unsignedWTxComponent match {
      case wtxP2SH: WitnessTxSigComponentP2SH =>
        WitnessTxSigComponent(signedSpendingTx,unsignedWTxComponent.inputIndex,
          wtxP2SH.scriptPubKey,unsignedWTxComponent.flags,unsignedWTxComponent.amount)
      case wtxRaw: WitnessTxSigComponentRaw =>
        WitnessTxSigComponent(signedSpendingTx,unsignedWTxComponent.inputIndex,
          wtxRaw.scriptPubKey,unsignedWTxComponent.flags,unsignedWTxComponent.amount)
    }

    (signedTxWitness, signedWtxSigComponent)
  }

  /** Creates a unsigned [[WitnessTxSigComponent]] from the given parameters */
  def createUnsignedWTxSigComponent(witScriptPubKey: WitnessScriptPubKey, amount: CurrencyUnit,
                                    unsignedScriptWitness: ScriptWitness, sequence: Option[UInt32]): WitnessTxSigComponent = {
    val witness = TransactionWitness(Seq(unsignedScriptWitness))
    val flags = Policy.standardScriptVerifyFlags
    val (creditingTx,outputIndex) = TransactionGenerators.buildCreditingTransaction(witScriptPubKey,amount)
    val (unsignedSpendingTx,inputIndex) = TransactionGenerators.buildSpendingTransaction(UInt32(2),creditingTx,
      EmptyScriptSignature, outputIndex, TransactionConstants.lockTime,
      sequence.getOrElse(TransactionConstants.sequence), witness)
    val unsignedWtxSigComponent = WitnessTxSigComponent(unsignedSpendingTx,inputIndex,witScriptPubKey,flags, amount)
    unsignedWtxSigComponent
  }

  def createUnsignedWTxSigComponent(witScriptPubKey: WitnessScriptPubKey, amount: CurrencyUnit,
                                    unsignedScriptWitness: ScriptWitness, sequence: Option[UInt32],
                                    outputs: Seq[TransactionOutput]): WitnessTxSigComponent = {
    val witness = TransactionWitness(Seq(unsignedScriptWitness))
    val flags = Policy.standardScriptVerifyFlags
    val (creditingTx,outputIndex) = TransactionGenerators.buildCreditingTransaction(witScriptPubKey,amount)
    val (unsignedSpendingTx,inputIndex) = TransactionGenerators.buildSpendingTransaction(UInt32(2),creditingTx,
      EmptyScriptSignature, outputIndex, TransactionConstants.lockTime,
      sequence.getOrElse(TransactionConstants.sequence), witness,outputs)
    val unsignedWtxSigComponent = WitnessTxSigComponent(unsignedSpendingTx,inputIndex,witScriptPubKey,flags, amount)
    unsignedWtxSigComponent
  }


  def createUnsignedWTxSigComponent(witScriptPubKey: WitnessScriptPubKey, amount: CurrencyUnit,
                                    unsignedScriptWitness: ScriptWitness, sequence: Option[UInt32],
                                    outputs: Seq[TransactionOutput],
                                    inputs: Seq[TransactionInput]): WitnessTxSigComponent = {
    val witness = TransactionWitness(Seq(unsignedScriptWitness))
    val flags = Policy.standardScriptVerifyFlags
    val (creditingTx,outputIndex) = TransactionGenerators.buildCreditingTransaction(witScriptPubKey,amount)
    val (unsignedSpendingTx,inputIndex) = TransactionGenerators.buildSpendingTransaction(UInt32(2),creditingTx,
      EmptyScriptSignature, outputIndex, TransactionConstants.lockTime,
      sequence.getOrElse(TransactionConstants.sequence), witness,outputs)
    val unsignedWtxSigComponent = WitnessTxSigComponent(unsignedSpendingTx,inputIndex,witScriptPubKey,flags, amount)
    unsignedWtxSigComponent
  }
}

object WTxSigComponentHelper extends WTxSigComponentHelper
