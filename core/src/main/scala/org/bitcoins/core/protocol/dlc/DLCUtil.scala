package org.bitcoins.core.protocol.dlc

import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script.P2WSHWitnessV0
import org.bitcoins.core.protocol.transaction.{Transaction, WitnessTransaction}
import org.bitcoins.crypto.{
  ECAdaptorSignature,
  ECDigitalSignature,
  ECPublicKey,
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
    * @param outcome A potential outcome that could have been executed for
    * @param adaptorSig The adaptor signature corresponding to outcome
    * @param cetSig The actual signature for local's key found on-chain on a CET
    */
  private def sigFromOutcomeAndSigs(
      outcome: OracleOutcome,
      adaptorSig: ECAdaptorSignature,
      cetSig: ECDigitalSignature): Try[SchnorrDigitalSignature] = {
    val sigPubKey = outcome.sigPoint

    // This value is either the oracle signature S value or it is
    // useless garbage, but we don't know in this scope, the caller
    // must do further work to check this.
    val possibleOracleST = Try {
      sigPubKey
        .extractAdaptorSecret(adaptorSig, ECDigitalSignature(cetSig.bytes.init))
        .fieldElement
    }

    possibleOracleST.map { possibleOracleS =>
      SchnorrDigitalSignature(outcome.aggregateNonce, possibleOracleS)
    }
  }

  def computeOutcome(
      completedSig: ECDigitalSignature,
      possibleAdaptorSigs: Vector[(OracleOutcome, ECAdaptorSignature)]): Option[
    (SchnorrDigitalSignature, OracleOutcome)] = {
    val sigOpt = possibleAdaptorSigs.find { case (outcome, adaptorSig) =>
      val possibleOracleSigT =
        sigFromOutcomeAndSigs(outcome, adaptorSig, completedSig)

      possibleOracleSigT.isSuccess && possibleOracleSigT.get.sig.getPublicKey == outcome.sigPoint
    }

    sigOpt.map { case (outcome, adaptorSig) =>
      (sigFromOutcomeAndSigs(outcome, adaptorSig, completedSig).get, outcome)
    }
  }

  def computeOutcome(
      isInitiator: Boolean,
      offerFundingKey: ECPublicKey,
      acceptFundingKey: ECPublicKey,
      contractInfo: ContractInfo,
      localAdaptorSigs: Vector[(OracleOutcome, ECAdaptorSignature)],
      cet: WitnessTransaction): Option[
    (SchnorrDigitalSignature, OracleOutcome)] = {
    val cetSigs = cet.witness.head
      .asInstanceOf[P2WSHWitnessV0]
      .signatures

    require(cetSigs.size == 2,
            s"There must be only 2 signatures, got ${cetSigs.size}")

    val outcomeValues = cet.outputs.map(_.value).sorted
    val totalCollateral = contractInfo.totalCollateral

    val possibleOutcomes = contractInfo.allOutcomesAndPayouts
      .filter { case (_, amt) =>
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
      .map(_._1)

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

    computeOutcome(cetSig, outcomeSigs)
  }
}
