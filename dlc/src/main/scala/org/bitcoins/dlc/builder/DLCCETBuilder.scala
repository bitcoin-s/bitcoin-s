package org.bitcoins.dlc.builder

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.dlc.{ContractInfo, DLCTimeouts, OracleOutcome}
import org.bitcoins.core.protocol.script.{
  EmptyScriptSignature,
  MultiSignatureScriptPubKey,
  P2WSHWitnessV0,
  ScriptPubKey
}
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.wallet.utxo.{
  ConditionalPath,
  InputInfo,
  P2WSHV0InputInfo
}
import org.bitcoins.crypto.ECPublicKey

/** Responsible for constructing unsigned
  * Contract Execution Transactions (CETs)
  */
case class DLCCETBuilder(
    contractInfo: ContractInfo,
    offerFundingKey: ECPublicKey,
    offerFinalSPK: ScriptPubKey,
    acceptFundingKey: ECPublicKey,
    acceptFinalSPK: ScriptPubKey,
    timeouts: DLCTimeouts,
    fundingOutputRef: OutputReference) {

  private val fundingOutPoint = fundingOutputRef.outPoint

  private val fundingInput = TransactionInput(
    fundingOutPoint,
    EmptyScriptSignature,
    TransactionConstants.disableRBFSequence)

  private val fundingKeys =
    Vector(offerFundingKey, acceptFundingKey).sortBy(_.hex)

  private val fundingInfo = P2WSHV0InputInfo(
    outPoint = fundingOutPoint,
    amount = fundingOutputRef.output.value,
    scriptWitness = P2WSHWitnessV0(MultiSignatureScriptPubKey(2, fundingKeys)),
    conditionalPath = ConditionalPath.NoCondition
  )

  private val witnesses = Vector(InputInfo.getScriptWitness(fundingInfo))
  private val witness = TransactionWitness.fromWitOpt(witnesses)

  private def cetOfferOutput(offerValue: Satoshis): TransactionOutput = {
    TransactionOutput(offerValue, offerFinalSPK)
  }

  private def cetAcceptOutput(acceptValue: Satoshis): TransactionOutput = {
    TransactionOutput(acceptValue, acceptFinalSPK)
  }

  /** Constructs a Contract Execution Transaction (CET)
    * for a given outcome
    */
  def buildCET(outcome: OracleOutcome): WitnessTransaction = {
    val (offerPayout, acceptPayout) = contractInfo.getPayouts(outcome)

    val outputs =
      Vector(cetOfferOutput(offerPayout), cetAcceptOutput(acceptPayout))
        .filter(_.value >= Policy.dustThreshold)

    WitnessTransaction(TransactionConstants.validLockVersion,
                       Vector(fundingInput),
                       outputs,
                       timeouts.contractMaturity.toUInt32,
                       witness)
  }
}
