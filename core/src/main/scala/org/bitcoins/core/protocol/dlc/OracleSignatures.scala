package org.bitcoins.core.protocol.dlc

import org.bitcoins.core.protocol.tlv.{DLCOutcomeType, UnsignedNumericOutcome}
import org.bitcoins.core.util.SeqWrapper
import org.bitcoins.crypto.{CryptoUtil, ECPrivateKey, SchnorrDigitalSignature}

/** Corresponds to a set of SchnorrDigitalSignatures given by a single oracle. */
sealed trait OracleSignatures extends SeqWrapper[SchnorrDigitalSignature] {

  /** This oracle's signatures */
  def sigs: Vector[SchnorrDigitalSignature]

  /** The SingleOracleInfo for the oracle whose signatures are stored here. */
  def oracle: SingleOracleInfo

  override def wrapped: Vector[SchnorrDigitalSignature] = sigs

  /** Verifies the signatures against a given outcome. */
  def verifySignatures(outcome: DLCOutcomeType): Boolean = {
    oracle.verifySigs(outcome, this)
  }

  /** Computes the sum of all signature s values. */
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

  /** Computes the aggregate s value from the given signatures to be used
    * in the given outcome.
    *
    * This is what is used to decrypt a CET adaptor signature.
    */
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

/** Wraps a single oracle signature of an Enum event. */
case class EnumOracleSignature(
    oracle: EnumSingleOracleInfo,
    sig: SchnorrDigitalSignature)
    extends OracleSignatures {
  override def sigs: Vector[SchnorrDigitalSignature] = Vector(sig)

  override def toString: String =
    s"EnumOracleSignature(${oracle.announcement.publicKey}, $sig)"
}

/** Wraps a set of oracle signatures of numeric digits. */
case class NumericOracleSignatures(
    oracle: NumericSingleOracleInfo,
    sigs: Vector[SchnorrDigitalSignature])
    extends OracleSignatures {

  /** Computes the NumericOutcome to which these signatures correspond. */
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
