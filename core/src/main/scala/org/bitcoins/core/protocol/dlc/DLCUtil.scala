package org.bitcoins.core.protocol.dlc

import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script.P2WSHWitnessV0
import org.bitcoins.core.protocol.transaction.{Transaction, WitnessTransaction}
import org.bitcoins.crypto.{
  ECAdaptorSignature,
  ECDigitalSignature,
  ECPublicKey,
  FieldElement,
  SchnorrDigitalSignature,
  Sha256Digest
}
import scodec.bits.ByteVector

import scala.util.Try

object DLCUtil {

  def computeContractId(
      fundingTx: Transaction,
      tempContractId: Sha256Digest): ByteVector = {
    fundingTx.txIdBE.bytes.xor(tempContractId.bytes)
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
}
