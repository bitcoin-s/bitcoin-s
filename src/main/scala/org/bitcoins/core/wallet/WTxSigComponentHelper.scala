package org.bitcoins.core.wallet

import org.bitcoins.core.crypto.{WitnessTxSigComponent, WitnessTxSigComponentP2SH, WitnessTxSigComponentRaw}
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.gen.TransactionGenerators
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._

/**
  * Created by chris on 5/9/17.
  */
sealed trait WTxSigComponentHelper {
  /** Takes a signed [[ScriptWitness]] and an unsignedTx and adds the witness to the unsigned [[WitnessTransaction]] */
  def createSignedWTxComponent(witness: ScriptWitness, unsignedWTxComponent: WitnessTxSigComponent): (TransactionWitness,WitnessTxSigComponent) = {
    val signedTxWitness = TransactionWitness.fromWitOpt(Seq(Some(witness)))
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
  def createUnsignedRawWTxSigComponent(witScriptPubKey: WitnessScriptPubKey, amount: CurrencyUnit,
                                    unsignedScriptWitness: ScriptWitness, sequence: Option[UInt32]): WitnessTxSigComponentRaw = {
    val tc = TransactionConstants
    val flags = Policy.standardScriptVerifyFlags
    val witness = TransactionWitness.fromWitOpt(Seq(Some(unsignedScriptWitness)))
    val (creditingTx,outputIndex) = TransactionGenerators.buildCreditingTransaction(witScriptPubKey,amount)
    val (unsignedSpendingTx,inputIndex) = TransactionGenerators.buildSpendingTransaction(tc.validLockVersion,creditingTx,
      EmptyScriptSignature, outputIndex, tc.lockTime,
      sequence.getOrElse(tc.sequence), witness)
    val unsignedWtxSigComponent = WitnessTxSigComponentRaw(unsignedSpendingTx,inputIndex,witScriptPubKey,flags, amount)
    unsignedWtxSigComponent
  }

  def createUnsignedP2SHWTxSigComponent(witSPK: WitnessScriptPubKey, amount: CurrencyUnit,
                                        unsignedScriptWitness: ScriptWitness, sequence: Option[UInt32],
                                        outputs: Seq[TransactionOutput],
                                        inputs: Seq[TransactionInput]): WitnessTxSigComponentP2SH = {
    val witness = TransactionWitness.fromWitOpt(Seq(Some(unsignedScriptWitness)))
    val flags = Policy.standardScriptVerifyFlags
    val tc = TransactionConstants
    val p2sh = P2SHScriptPubKey(witSPK)
    val (creditingTx,outputIndex) = TransactionGenerators.buildCreditingTransaction(witSPK,amount)
    val p2shScriptSig = P2SHScriptSignature(witSPK)
    val (unsignedSpendingTx,inputIndex) = TransactionGenerators.buildSpendingTransaction(tc.validLockVersion,creditingTx,
      p2shScriptSig, outputIndex, tc.lockTime,
      sequence.getOrElse(tc.sequence), witness,outputs)
    val unsignedWtxSigComponent = WitnessTxSigComponentP2SH(unsignedSpendingTx,inputIndex,p2sh,flags, amount)
    unsignedWtxSigComponent
  }
}

object WTxSigComponentHelper extends WTxSigComponentHelper
