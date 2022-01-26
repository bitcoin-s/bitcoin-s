package org.bitcoins.core.protocol.dlc.compute

import org.bitcoins.core.number.UInt16
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.dlc.build.DLCTxBuilder
import org.bitcoins.core.protocol.dlc.models.DLCMessage.{
  DLCAccept,
  DLCAcceptWithoutSigs,
  DLCOffer
}
import org.bitcoins.core.protocol.dlc.models.{ContractInfo, OracleOutcome}
import org.bitcoins.core.protocol.script.P2WSHWitnessV0
import org.bitcoins.core.protocol.transaction.{Transaction, WitnessTransaction}
import org.bitcoins.crypto._
import scodec.bits.ByteVector

import scala.util.Try

object DLCUtil {

  /** @see https://github.com/discreetlogcontracts/dlcspecs/blob/master/Protocol.md#definition-of-contract_id
    * @param fundingTx the transaction that contains the DLC funding output
    * @param outputIdx the index of the output
    * @param tempContractId the temporary contractId in the offer message
    * @return
    */
  def computeContractId(
      fundingTx: Transaction,
      outputIdx: Int,
      tempContractId: Sha256Digest): ByteVector = {
    val u16 = UInt16(outputIdx)
    //we need to pad the u16 due to how xor works in scodec so we don't lose precision
    val padded = ByteVector.fill(30)(0.toByte) ++ u16.bytes
    fundingTx.txIdBE.bytes
      .xor(tempContractId.bytes)
      .xor(padded)
  }

  /** Extracts an adaptor secret from cetSig assuming it is the completion
    * adaptorSig (which it may not be) and returns the oracle signature if
    * and only if adaptorSig does correspond to cetSig.
    *
    * This method is used to search through possible cetSigs until the correct
    * one is found by validating the returned signature.
    *
    * @param adaptorPoint A potential adaptor point that could have been executed for
    * @param adaptorSig The adaptor signature corresponding to outcome
    * @param cetSig The actual signature for local's key found on-chain on a CET
    */
  private def sigFromOutcomeAndSigs(
      adaptorPoint: ECPublicKey,
      adaptorSig: ECAdaptorSignature,
      cetSig: ECDigitalSignature): Try[FieldElement] = {
    // This value is either the oracle signature S value or it is
    // useless garbage, but we don't know in this scope, the caller
    // must do further work to check this.
    Try {
      adaptorPoint
        .extractAdaptorSecret(adaptorSig, ECDigitalSignature(cetSig.bytes.init))
        .fieldElement
    }
  }

  def computeOutcome(
      completedSig: ECDigitalSignature,
      possibleAdaptorSigs: Vector[(ECPublicKey, ECAdaptorSignature)]): Option[
    (FieldElement, ECPublicKey)] = {
    val sigOpt = possibleAdaptorSigs.find { case (adaptorPoint, adaptorSig) =>
      val possibleOracleSigT =
        sigFromOutcomeAndSigs(adaptorPoint, adaptorSig, completedSig)

      possibleOracleSigT.isSuccess && possibleOracleSigT.get.getPublicKey == adaptorPoint
    }

    sigOpt.map { case (adaptorPoint, adaptorSig) =>
      (sigFromOutcomeAndSigs(adaptorPoint, adaptorSig, completedSig).get,
       adaptorPoint)
    }
  }

  def computeOutcome(
      isInitiator: Boolean,
      offerFundingKey: ECPublicKey,
      acceptFundingKey: ECPublicKey,
      contractInfo: ContractInfo,
      localAdaptorSigs: Vector[(ECPublicKey, ECAdaptorSignature)],
      cet: WitnessTransaction): Option[
    (SchnorrDigitalSignature, OracleOutcome)] = {
    val allAdaptorPoints = contractInfo.adaptorPoints

    val cetSigs = cet.witness.head
      .asInstanceOf[P2WSHWitnessV0]
      .signatures

    require(cetSigs.size == 2,
            s"There must be only 2 signatures, got ${cetSigs.size}")

    val outcomeValues = cet.outputs.map(_.value).sorted
    val totalCollateral = contractInfo.totalCollateral

    val possibleOutcomes = contractInfo.allOutcomesAndPayouts.zipWithIndex
      .filter { case ((_, amt), _) =>
        val amts = Vector(amt, totalCollateral - amt)
          .filter(_ >= Policy.dustThreshold)
          .sorted

        // Only messages within 1 satoshi of the on-chain CET's value
        // should be considered.
        // Off-by-one is okay because both parties round to the nearest
        // Satoshi for fees and if both round up they could be off-by-one.
        Math.abs((amts.head - outcomeValues.head).satoshis.toLong) <= 1 && Math
          .abs((amts.last - outcomeValues.last).satoshis.toLong) <= 1
      }
      .map { case (_, index) => allAdaptorPoints(index) }

    val (offerCETSig, acceptCETSig) =
      if (offerFundingKey.hex.compareTo(acceptFundingKey.hex) > 0) {
        (cetSigs.last, cetSigs.head)
      } else {
        (cetSigs.head, cetSigs.last)
      }

    val outcomeSigs = localAdaptorSigs.filter { case (outcome, _) =>
      possibleOutcomes.contains(outcome)
    }

    val cetSig = if (isInitiator) {
      acceptCETSig
    } else {
      offerCETSig
    }

    computeOutcome(cetSig, outcomeSigs).map { case (s, adaptorPoint) =>
      val index = allAdaptorPoints.indexOf(adaptorPoint)
      val outcome: OracleOutcome = contractInfo.allOutcomes(index)

      (SchnorrDigitalSignature(outcome.aggregateNonce, s), outcome)
    }
  }

  def calcContractId(offer: DLCOffer, accept: DLCAccept): ByteVector = {
    calcContractId(offer, accept.withoutSigs)
  }

  def calcContractId(
      offer: DLCOffer,
      acceptWithoutSigs: DLCAcceptWithoutSigs): ByteVector = {
    val fundingKeys =
      Vector(offer.pubKeys.fundingKey, acceptWithoutSigs.pubKeys.fundingKey)
    val fundOutputSerialId = offer.fundOutputSerialId
    val offerFundingInputs = offer.fundingInputs
    val acceptFundingInputs = acceptWithoutSigs.fundingInputs
    val offerChangeSPK = offer.changeAddress.scriptPubKey
    val acceptChangeSPK = acceptWithoutSigs.changeAddress.scriptPubKey
    val offerFinalAddressSPK = offer.pubKeys.payoutAddress.scriptPubKey
    val acceptFinalAddressSPK =
      acceptWithoutSigs.pubKeys.payoutAddress.scriptPubKey
    val feeRate = offer.feeRate
    val (_, fundingSPK) = DLCTxBuilder.buildFundingSPKs(fundingKeys)

    val fundingTxFinalizer = DLCTxBuilder.buildFundingTxFinalizer(
      offerFundingInputs = offerFundingInputs,
      acceptFundingInputs = acceptFundingInputs,
      offerChangeSPK = offerChangeSPK,
      acceptChangeSPK = acceptChangeSPK,
      offerPayoutSPK = offerFinalAddressSPK,
      acceptPayoutSPK = acceptFinalAddressSPK,
      feeRate = feeRate,
      fundingSPK = fundingSPK
    )

    val (fundingTx, fundingOutputIdx) = DLCTxBuilder.buildFundingTransaction(
      offerInput = offer.totalCollateral,
      acceptInput = acceptWithoutSigs.totalCollateral,
      offerFundingInputs = offerFundingInputs,
      acceptFundingInputs = acceptFundingInputs,
      offerChangeSPK = offerChangeSPK,
      offerChangeSerialId = offer.changeSerialId,
      acceptChangeSPK = acceptChangeSPK,
      acceptChangeSerialId = acceptWithoutSigs.changeSerialId,
      fundingSPK = fundingSPK,
      fundOutputSerialId = fundOutputSerialId,
      finalizer = fundingTxFinalizer
    )

    computeContractId(fundingTx = fundingTx,
                      outputIdx = fundingOutputIdx,
                      tempContractId = offer.tempContractId)
  }
}
