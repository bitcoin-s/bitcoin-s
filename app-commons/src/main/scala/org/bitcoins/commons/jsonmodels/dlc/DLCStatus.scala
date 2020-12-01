package org.bitcoins.commons.jsonmodels.dlc

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage._
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script.P2WSHWitnessV0
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.protocol.transaction.{Transaction, WitnessTransaction}
import org.bitcoins.crypto._

object DLCStatus {

  def calculateOutcomeAndSig(
      isInitiator: Boolean,
      offer: DLCOffer,
      accept: DLCAccept,
      sign: DLCSign,
      cet: Transaction): (SchnorrDigitalSignature, DLCOutcomeType) = {
    val cetSigs = cet
      .asInstanceOf[WitnessTransaction]
      .witness
      .head
      .asInstanceOf[P2WSHWitnessV0]
      .signatures

    require(cetSigs.size == 2,
            s"There must be only 2 signatures, got ${cetSigs.size}")

    val oraclePubKey = offer.oracleInfo.pubKey
    val rVals = offer.oracleInfo.nonces

    def aggregateR(numSigs: Int): SchnorrNonce = {
      rVals.take(numSigs).map(_.publicKey).reduce(_.add(_)).schnorrNonce
    }

    def sigFromMsgAndSigs(
        outcome: DLCOutcomeType,
        adaptorSig: ECAdaptorSignature,
        cetSig: ECDigitalSignature): SchnorrDigitalSignature = {
      val (sigPubKey, numSigs) = outcome match {
        case EnumOutcome(outcome) =>
          val sigPoint = oraclePubKey.computeSigPoint(
            CryptoUtil.sha256(outcome).bytes,
            aggregateR(1))

          (sigPoint, 1)
        case UnsignedNumericOutcome(digits) =>
          val sigPoint = digits
            .zip(rVals.take(digits.length))
            .map {
              case (digit, nonce) =>
                oraclePubKey.computeSigPoint(
                  CryptoUtil.sha256(digit.toString).bytes,
                  nonce)
            }
            .reduce(_.add(_))

          (sigPoint, digits.length)
      }

      val possibleOracleS =
        sigPubKey
          .extractAdaptorSecret(adaptorSig,
                                ECDigitalSignature(cetSig.bytes.init))
          .fieldElement
      SchnorrDigitalSignature(aggregateR(numSigs), possibleOracleS)
    }

    val outcomeValues = cet.outputs.map(_.value).sorted
    val totalCollateral = offer.totalCollateral + accept.totalCollateral

    val possibleMessages = offer.contractInfo match {
      case DLCMessage.SingleNonceContractInfo(outcomeValueMap) =>
        outcomeValueMap
          .filter {
            case (_, amt) =>
              Vector(amt, totalCollateral - amt)
                .filter(_ >= Policy.dustThreshold)
                .sorted == outcomeValues
          }
          .map(_._1)
      case info: DLCMessage.MultiNonceContractInfo =>
        info.outcomeVec
          .filter {
            case (_, amt) =>
              val amts = Vector(amt, totalCollateral - amt)
                .filter(_ >= Policy.dustThreshold)
                .sorted

              Math.abs(
                (amts.head - outcomeValues.head).satoshis.toLong) <= 1 && Math
                .abs((amts.last - outcomeValues.last).satoshis.toLong) <= 1
          }
          .map { case (digits, _) => UnsignedNumericOutcome(digits) }
    }

    val (offerCETSig, acceptCETSig) =
      if (
        offer.pubKeys.fundingKey.hex.compareTo(
          accept.pubKeys.fundingKey.hex) > 0
      ) {
        (cetSigs.last, cetSigs.head)
      } else {
        (cetSigs.head, cetSigs.last)
      }

    val (cetSig, outcomeSigs) = if (isInitiator) {
      val possibleOutcomeSigs = sign.cetSigs.outcomeSigs.filter {
        case (msg, _) => possibleMessages.contains(msg)
      }
      (acceptCETSig, possibleOutcomeSigs)
    } else {
      val possibleOutcomeSigs = accept.cetSigs.outcomeSigs.filter {
        case (msg, _) => possibleMessages.contains(msg)
      }
      (offerCETSig, possibleOutcomeSigs)
    }

    val sigOpt = outcomeSigs.find {
      case (outcome, adaptorSig) =>
        val possibleOracleSig = sigFromMsgAndSigs(outcome, adaptorSig, cetSig)
        val sigPoint = offer.oracleAndContractInfo.sigPointForOutcome(outcome)
        possibleOracleSig.sig.getPublicKey == sigPoint
    }

    sigOpt match {
      case Some((msg, adaptorSig)) =>
        (sigFromMsgAndSigs(msg, adaptorSig, cetSig), msg)
      case None =>
        throw new IllegalArgumentException("No Oracle Signature found from CET")
    }
  }
}
