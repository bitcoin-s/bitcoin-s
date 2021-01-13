package org.bitcoins.core.protocol.dlc

import org.bitcoins.core.protocol.tlv.{DLCOutcomeType, UnsignedNumericOutcome}
import org.bitcoins.core.util.SeqWrapper
import org.bitcoins.crypto.{CryptoUtil, ECPrivateKey, SchnorrDigitalSignature}

sealed trait OracleSignatures extends SeqWrapper[SchnorrDigitalSignature] {
  def sigs: Vector[SchnorrDigitalSignature]

  def oracle: SingleOracleInfo

  override def wrapped: Vector[SchnorrDigitalSignature] = sigs

  def verifySignatures(outcome: DLCOutcomeType): Boolean = {
    oracle.verifySigs(outcome, this)
  }

  def aggregateSig(outcome: DLCOutcomeType): ECPrivateKey = {
    sigs
      .take(outcome.serialized.length)
      .map(_.sig)
      .reduce(_.add(_))
      .toPrivateKey
  }
}

object OracleSignatures {

  def apply(
      oracle: SingleOracleInfo,
      sigs: Vector[SchnorrDigitalSignature]): OracleSignatures = {
    oracle match {
      case info: EnumSingleOracleInfo =>
        require(sigs.length == 1, s"Expected one signature, got $sigs")
        EnumOracleSignature(info, sigs.head)
      case info: NumericSingleOracleInfo =>
        NumericOracleSignatures(info, sigs)
    }
  }

  def computeAggregateSignature(
      outcome: OracleOutcome,
      sigs: Vector[OracleSignatures]): ECPrivateKey = {
    outcome match {
      case EnumOracleOutcome(_, enumOutcome) =>
        sigs.map(_.aggregateSig(enumOutcome)).reduce(_.add(_))
      case NumericOracleOutcome(oraclesAndOutcomes) =>
        sigs
          .map { sig =>
            val numericOutcome =
              oraclesAndOutcomes.find(_._1 == sig.oracle).get._2
            sig.aggregateSig(numericOutcome)
          }
          .reduce(_.add(_))
    }
  }
}

case class EnumOracleSignature(
    oracle: EnumSingleOracleInfo,
    sig: SchnorrDigitalSignature)
    extends OracleSignatures {
  override def sigs: Vector[SchnorrDigitalSignature] = Vector(sig)

  override def toString: String =
    s"EnumOracleSignature(${oracle.announcement.publicKey}, $sig)"
}

case class NumericOracleSignatures(
    oracle: NumericSingleOracleInfo,
    sigs: Vector[SchnorrDigitalSignature])
    extends OracleSignatures {

  def computeOutcome(
      base: Int,
      possibleOutcomes: Vector[DLCOutcomeType]): Option[
    UnsignedNumericOutcome] = {
    val digitsSigned = sigs.map { sig =>
      (0 until base)
        .find { possibleDigit =>
          oracle.publicKey
            .verify(CryptoUtil
                      .sha256DLCAttestation(possibleDigit.toString)
                      .bytes,
                    sig)
        }
        .getOrElse(throw new IllegalArgumentException(
          s"Signature $sig does not match any digit 0-${base - 1}"))
    }

    CETCalculator.searchForNumericOutcome(digitsSigned, possibleOutcomes)
  }

  override def toString: String =
    s"NumericOracleSignatures(${oracle.announcement.publicKey}, $sigs)"
}
