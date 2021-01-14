package org.bitcoins.core.protocol.dlc

import org.bitcoins.core.protocol.tlv.{
  DLCOutcomeType,
  EnumOutcome,
  UnsignedNumericOutcome
}
import org.bitcoins.crypto.{ECPublicKey, SchnorrNonce}

sealed trait OracleOutcome {
  def oracles: Vector[SingleOracleInfo]
  def outcome: DLCOutcomeType
  def sigPoint: ECPublicKey
  def aggregateNonce: SchnorrNonce
}

case class EnumOracleOutcome(
    oracles: Vector[EnumSingleOracleInfo],
    outcome: EnumOutcome)
    extends OracleOutcome {

  override lazy val sigPoint: ECPublicKey = {
    oracles.map(_.sigPoint(outcome)).reduce(_.add(_))
  }

  override lazy val aggregateNonce: SchnorrNonce = {
    oracles
      .map(_.aggregateNonce(outcome))
      .map(_.publicKey)
      .reduce(_.add(_))
      .schnorrNonce
  }
}

case class NumericOracleOutcome(oraclesAndOutcomes: Vector[
  (NumericSingleOracleInfo, UnsignedNumericOutcome)])
    extends OracleOutcome {

  override def oracles: Vector[NumericSingleOracleInfo] = {
    oraclesAndOutcomes.map(_._1)
  }

  override def outcome: UnsignedNumericOutcome = {
    oraclesAndOutcomes.head._2
  }

  override lazy val sigPoint: ECPublicKey = {
    oraclesAndOutcomes
      .map {
        case (oracle, outcome) =>
          oracle.sigPoint(outcome)
      }
      .reduce(_.add(_))
  }

  override lazy val aggregateNonce: SchnorrNonce = {
    oraclesAndOutcomes
      .map {
        case (oracle, outcome) =>
          oracle.aggregateNonce(outcome)
      }
      .map(_.publicKey)
      .reduce(_.add(_))
      .schnorrNonce
  }
}

object NumericOracleOutcome {

  def apply(
      oracleInfo: NumericSingleOracleInfo,
      outcome: UnsignedNumericOutcome): NumericOracleOutcome = {
    NumericOracleOutcome(Vector((oracleInfo, outcome)))
  }
}
