package org.bitcoins.dlc.builder

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage.OracleAndContractInfo
import org.bitcoins.commons.jsonmodels.dlc.DLCTimeouts
import org.bitcoins.core.protocol.script.{
  EmptyScriptSignature,
  MultiSignatureScriptPubKey,
  P2WSHWitnessV0,
  ScriptPubKey
}
import org.bitcoins.core.protocol.tlv.DLCOutcomeType
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.wallet.builder.{
  AddWitnessDataFinalizer,
  FilterDustFinalizer,
  RawTxBuilder
}
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.utxo.{ConditionalPath, P2WSHV0InputInfo}
import org.bitcoins.crypto.ECPublicKey

import scala.concurrent.{ExecutionContext, Future}

/** Responsible for constructing unsigned
  * Contract Execution Transactions (CETs)
  */
case class DLCCETBuilder[Outcome <: DLCOutcomeType](
    oracleAndContractInfo: OracleAndContractInfo[Outcome],
    offerFundingKey: ECPublicKey,
    offerFinalSPK: ScriptPubKey,
    acceptFundingKey: ECPublicKey,
    acceptFinalSPK: ScriptPubKey,
    timeouts: DLCTimeouts,
    feeRate: FeeUnit,
    fundingOutputRef: OutputReference) {

  private val fundingOutPoint = fundingOutputRef.outPoint

  private val fundingKeys =
    Vector(offerFundingKey, acceptFundingKey).sortBy(_.hex)

  private val fundingInfo = P2WSHV0InputInfo(
    outPoint = fundingOutPoint,
    amount = fundingOutputRef.output.value,
    scriptWitness = P2WSHWitnessV0(MultiSignatureScriptPubKey(2, fundingKeys)),
    conditionalPath = ConditionalPath.NoCondition
  )

  /** Constructs a Contract Execution Transaction (CET)
    * for a given outcome hash
    */
  def buildCET(msg: Outcome)(implicit
      ec: ExecutionContext): Future[WitnessTransaction] = {
    val builder = RawTxBuilder().setLockTime(timeouts.contractMaturity.toUInt32)

    val (offerValue, acceptValue) = oracleAndContractInfo.getPayouts(msg)

    builder += TransactionOutput(offerValue, offerFinalSPK)
    builder += TransactionOutput(acceptValue, acceptFinalSPK)

    builder += TransactionInput(fundingOutPoint,
                                EmptyScriptSignature,
                                TransactionConstants.disableRBFSequence)

    val finalizer =
      FilterDustFinalizer
        .andThen(AddWitnessDataFinalizer(Vector(fundingInfo)))

    val txF = finalizer.buildTx(builder.result())

    txF.flatMap {
      case _: NonWitnessTransaction =>
        Future.failed(
          new RuntimeException(
            "Something went wrong with AddWitnessDataFinalizer"))
      case wtx: WitnessTransaction => Future.successful(wtx)
    }
  }
}
