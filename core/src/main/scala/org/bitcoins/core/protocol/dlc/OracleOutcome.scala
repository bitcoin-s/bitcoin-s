package org.bitcoins.core.protocol.dlc

import org.bitcoins.core.protocol.tlv.{
  DLCOutcomeType,
  EnumOutcome,
  UnsignedNumericOutcome
}
import org.bitcoins.core.protocol.transaction.WitnessTransaction
import org.bitcoins.crypto.{ECPublicKey, SchnorrNonce}

/** OracleOutcomes are in one-to-one correspondence with Contract
  * Execution Transactions (CETs) and are defined by a set of oracles
  * needed to execute with a given CET, representing a certain outcome
  * and using a certain signature point (aka adaptor point).
  */
sealed trait OracleOutcome {

  /** The oracles whose signatures are needed for execution with this outcome. */
  def oracles: Vector[SingleOracleInfo]

  /** The DLCOutcomeType this OracleOutcome corresponds to (from a payout perspective).
    *
    * Note that for the case of multi-oracle numeric outcomes with bounded differences
    * allowed between oracles, this corresponds to the primary oracle's outcome.
    */
  def outcome: DLCOutcomeType

  protected def computeSigPoint: ECPublicKey

  /** The adaptor point used to encrypt the signatures for this corresponding CET. */
  def sigPoint: ECPublicKey = {
    SigPointComputer(() => computeSigPoint).compute
  }

  /** The sum of all oracle nonces used in execution with this OracleOutcome. */
  def aggregateNonce: SchnorrNonce
}

/** Corresponds to a CET in an Enumerated Outcome DLC where some set of `threshold`
  * oracles have signed a given EnumOutcome.
  */
case class EnumOracleOutcome(
    oracles: Vector[EnumSingleOracleInfo],
    outcome: EnumOutcome)
    extends OracleOutcome {

  override protected def computeSigPoint: ECPublicKey = {
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

/** Corresponds to a CET in an Numeric Outcome DLC where some set of `threshold`
  * oracles have each signed some NumericOutcome.
  */
case class NumericOracleOutcome(oraclesAndOutcomes: Vector[
  (NumericSingleOracleInfo, UnsignedNumericOutcome)])
    extends OracleOutcome {

  override def oracles: Vector[NumericSingleOracleInfo] = {
    oraclesAndOutcomes.map(_._1)
  }

  override def outcome: UnsignedNumericOutcome = {
    oraclesAndOutcomes.head._2
  }

  def outcomes: Vector[UnsignedNumericOutcome] =
    oraclesAndOutcomes.map(_._2)

  override protected def computeSigPoint: ECPublicKey = {
    oraclesAndOutcomes
      .map { case (oracle, outcome) =>
        oracle.sigPoint(outcome)
      }
      .reduce(_.add(_))
  }

  override lazy val aggregateNonce: SchnorrNonce = {
    oraclesAndOutcomes
      .map { case (oracle, outcome) =>
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

/** An oracle outcome and it's corresponding CET */
case class OutcomeCETPair(outcome: OracleOutcome, wtx: WitnessTransaction)
