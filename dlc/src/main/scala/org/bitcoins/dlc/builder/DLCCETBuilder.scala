package org.bitcoins.dlc.builder

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage.{ContractInfo, OracleInfo}
import org.bitcoins.commons.jsonmodels.dlc.DLCTimeouts
import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.constant.ScriptNumber
import org.bitcoins.core.wallet.builder.{
  AddWitnessDataFinalizer,
  FilterDustFinalizer,
  RawTxBuilder
}
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.utxo.{ConditionalPath, P2WSHV0InputInfo}
import org.bitcoins.crypto._

import scala.concurrent.{ExecutionContext, Future}

case class DLCCETBuilder(
    offerOutcomes: ContractInfo,
    acceptOutcomes: ContractInfo,
    offerFundingKey: ECPublicKey,
    offerToLocalKey: ECPublicKey,
    offerFinalSPK: ScriptPubKey,
    acceptFundingKey: ECPublicKey,
    acceptToLocalKey: ECPublicKey,
    acceptFinalSPK: ScriptPubKey,
    timeouts: DLCTimeouts,
    feeRate: FeeUnit,
    oracleInfo: OracleInfo,
    fundingOutputRef: OutputReference) {

  val OracleInfo(oraclePubKey: SchnorrPublicKey, preCommittedR: SchnorrNonce) =
    oracleInfo

  val sigPubKeys: Map[Sha256DigestBE, ECPublicKey] = offerOutcomes.keys.map {
    msg =>
      msg -> oraclePubKey.computeSigPoint(msg.bytes, preCommittedR)
  }.toMap

  private val fundingOutPoint = fundingOutputRef.outPoint

  private val fundingInfo = P2WSHV0InputInfo(
    outPoint = fundingOutPoint,
    amount = fundingOutputRef.output.value,
    scriptWitness = P2WSHWitnessV0(
      MultiSignatureScriptPubKey(2, Vector(offerFundingKey, acceptFundingKey))),
    conditionalPath = ConditionalPath.NoCondition
  )

  private def buildToLocalP2PK(
      msg: Sha256DigestBE,
      fundingPubKey: ECPublicKey,
      toLocalCETKey: ECPublicKey,
      otherToLocalCETKey: ECPublicKey): P2PKWithTimeoutScriptPubKey = {
    val tweak = CryptoUtil.sha256(toLocalCETKey.bytes).flip

    val tweakPubKey = ECPrivateKey.fromBytes(tweak.bytes).publicKey

    val pubKey = sigPubKeys(msg).add(fundingPubKey).add(tweakPubKey)

    P2PKWithTimeoutScriptPubKey(
      pubKey = pubKey,
      lockTime = ScriptNumber(timeouts.penaltyTimeout.toLong),
      timeoutPubKey = otherToLocalCETKey
    )
  }

  def buildOfferToLocalP2PK(
      msg: Sha256DigestBE): P2PKWithTimeoutScriptPubKey = {
    buildToLocalP2PK(msg, offerFundingKey, offerToLocalKey, acceptToLocalKey)
  }

  def buildAcceptToLocalP2PK(
      msg: Sha256DigestBE): P2PKWithTimeoutScriptPubKey = {
    buildToLocalP2PK(msg, acceptFundingKey, acceptToLocalKey, offerToLocalKey)
  }

  def buildCET(msg: Sha256DigestBE, isOffer: Boolean)(
      implicit ec: ExecutionContext): Future[WitnessTransaction] = {
    val builder = RawTxBuilder().setLockTime(timeouts.contractMaturity.toUInt32)

    val toLocalSPK = if (isOffer) {
      P2WSHWitnessSPKV0(buildOfferToLocalP2PK(msg))
    } else {
      P2WSHWitnessSPKV0(buildAcceptToLocalP2PK(msg))
    }

    val toLocalClosingFee = Satoshis(
      feeRate.toLong * DLCTxBuilder.approxToLocalClosingVBytes)

    val (toLocalValue, toRemoteValue) = if (isOffer) {
      (offerOutcomes(msg), acceptOutcomes(msg))
    } else {
      (acceptOutcomes(msg), offerOutcomes(msg))
    }

    val toRemoteSPK = if (isOffer) {
      acceptFinalSPK
    } else {
      offerFinalSPK
    }

    builder += TransactionOutput(toLocalValue + toLocalClosingFee, toLocalSPK)
    builder += TransactionOutput(toRemoteValue, toRemoteSPK)

    builder += TransactionInput(fundingOutPoint,
                                EmptyScriptSignature,
                                TransactionConstants.disableRBFSequence)

    val finalizer =
      FilterDustFinalizer.andThen(AddWitnessDataFinalizer(Vector(fundingInfo)))

    val txF = finalizer.buildTx(builder.result())

    txF.flatMap {
      case _: NonWitnessTransaction =>
        Future.failed(
          new RuntimeException(
            "Something went wrong with AddWitnessDataFinalizer"))
      case wtx: WitnessTransaction => Future.successful(wtx)
    }
  }

  def buildOfferCET(msg: Sha256DigestBE)(
      implicit ec: ExecutionContext): Future[WitnessTransaction] = {
    buildCET(msg, isOffer = true)
  }

  def buildAcceptCET(msg: Sha256DigestBE)(
      implicit ec: ExecutionContext): Future[WitnessTransaction] = {
    buildCET(msg, isOffer = false)
  }
}
