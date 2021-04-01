package org.bitcoins.core.protocol.dlc

import org.bitcoins.core.currency._
import org.bitcoins.core.protocol.dlc.ContractOraclePair._
import org.bitcoins.core.protocol.dlc.DLCTemplate._
import org.bitcoins.core.protocol.tlv._

sealed trait DLCTemplate {
  def totalCollateral: CurrencyUnit

  def oracles: Vector[OracleAnnouncementTLV]

  def oracleThreshold: Int

  def contractDescriptor: ContractDescriptor

  def oracleInfo: OracleInfo

  def toContractInfo: ContractInfo

  def individualCollateral: CurrencyUnit
}

sealed trait SingleOracleDLCTemplate extends DLCTemplate {
  def oracle: OracleAnnouncementTLV

  override def oracleInfo: SingleOracleInfo

  final override def oracleThreshold: Int = 1
  final override def oracles: Vector[OracleAnnouncementTLV] = Vector(oracle)
}

sealed trait MultiOracleDLCTemplate extends DLCTemplate {
  override def oracleInfo: MultiOracleInfo[SingleOracleInfo]
}

/** A template for creating a Contract For Difference DLC
  *
  * @see https://www.investopedia.com/terms/c/contractfordifferences.asp
  */
sealed trait CFDTemplate extends DLCTemplate {
  override def oracleInfo: NumericOracleInfo

  def strikePrice: Long

  def roundingIntervals: RoundingIntervals

  def isLong: Boolean

  override val contractDescriptor: NumericContractDescriptor = {
    oracles.head.eventTLV.eventDescriptor match {
      case _: EnumEventDescriptorV0TLV =>
        throw new IllegalArgumentException(
          "Cannot do a CFD with a EnumEventDescriptorV0TLV")
      case decomp: DigitDecompositionEventDescriptorV0TLV =>
        require(validateMatchingOracleDescriptors(oracles),
                "Oracles must use same numDigits and base")

        val func: Long => Long = { outcome =>
          if (outcome == 0) Long.MaxValue
          else (strikePrice * individualCollateral.satoshis.toLong) / outcome
        }

        val numDigits = decomp.numDigits.toInt

        // TODO use hyperbola DLC
        val curve = CETCalculator.lineApprox(func, numDigits, 100)

        val descriptor = NumericContractDescriptor(curve,
                                                   numDigits = numDigits,
                                                   roundingIntervals)

        if (isLong) {
          descriptor.flip(totalCollateral.satoshis)
        } else descriptor
    }
  }

  override val toContractInfo: ContractInfo = {
    val pair: NumericPair =
      ContractOraclePair.NumericPair(contractDescriptor, oracleInfo)
    ContractInfo(totalCollateral.satoshis, pair)
  }
}

/** @inheritdoc */
case class SingleOracleLongCFD(
    individualCollateral: CurrencyUnit,
    totalCollateral: CurrencyUnit,
    oracle: OracleAnnouncementTLV,
    strikePrice: Long,
    roundingIntervals: RoundingIntervals
) extends SingleOracleDLCTemplate
    with CFDTemplate {

  override val oracleInfo: NumericSingleOracleInfo = NumericSingleOracleInfo(
    oracle)

  override def isLong: Boolean = true
}

/** @inheritdoc */
case class MultiOracleLongCFD(
    individualCollateral: CurrencyUnit,
    totalCollateral: CurrencyUnit,
    oracles: Vector[OracleAnnouncementTLV],
    oracleThreshold: Int,
    strikePrice: Long,
    maxErrorExp: Int,
    minFailExp: Int,
    maximizeCoverage: Boolean,
    roundingIntervals: RoundingIntervals
) extends MultiOracleDLCTemplate
    with CFDTemplate {

  require(oracles.nonEmpty, "Cannot have no oracles")
  require(oracleThreshold > 0, "Oracle threshold must be greater than 0")
  require(
    oracles.size >= oracleThreshold,
    s"Oracle threshold cannot be greater than the number of oracles, got ${oracles.size} >= $oracleThreshold")

  override val oracleInfo: NumericMultiOracleInfo =
    NumericMultiOracleInfo(threshold = oracleThreshold,
                           announcements = oracles,
                           maxErrorExp = maxErrorExp,
                           minFailExp = minFailExp,
                           maximizeCoverage = maximizeCoverage)

  override def isLong: Boolean = true
}

/** @inheritdoc */
case class SingleOracleShortCFD(
    individualCollateral: CurrencyUnit,
    totalCollateral: CurrencyUnit,
    oracle: OracleAnnouncementTLV,
    strikePrice: Long,
    roundingIntervals: RoundingIntervals
) extends SingleOracleDLCTemplate
    with CFDTemplate {

  override val oracleInfo: NumericSingleOracleInfo = NumericSingleOracleInfo(
    oracle)

  override def isLong: Boolean = false
}

/** @inheritdoc */
case class MultiOracleShortCFD(
    individualCollateral: CurrencyUnit,
    totalCollateral: CurrencyUnit,
    oracles: Vector[OracleAnnouncementTLV],
    oracleThreshold: Int,
    strikePrice: Long,
    maxErrorExp: Int,
    minFailExp: Int,
    maximizeCoverage: Boolean,
    roundingIntervals: RoundingIntervals
) extends MultiOracleDLCTemplate
    with CFDTemplate {

  require(oracles.nonEmpty, "Cannot have no oracles")
  require(oracleThreshold > 0, "Oracle threshold must be greater than 0")
  require(
    oracles.size >= oracleThreshold,
    s"Oracle threshold cannot be greater than the number of oracles, got ${oracles.size} >= $oracleThreshold")

  override val oracleInfo: NumericMultiOracleInfo =
    NumericMultiOracleInfo(threshold = oracleThreshold,
                           announcements = oracles,
                           maxErrorExp = maxErrorExp,
                           minFailExp = minFailExp,
                           maximizeCoverage = maximizeCoverage)

  override def isLong: Boolean = false
}

/** A template for doing an options contract DLC
  *
  * @see https://www.investopedia.com/terms/o/optionscontract.asp
  */
sealed trait OptionTemplate extends DLCTemplate {
  override def oracleInfo: NumericOracleInfo

  def premium: CurrencyUnit

  def strikePrice: Long

  def isCall: Boolean

  def roundingIntervals: RoundingIntervals

  override val contractDescriptor: NumericContractDescriptor = {
    oracles.head.eventTLV.eventDescriptor match {
      case _: EnumEventDescriptorV0TLV =>
        throw new IllegalArgumentException(
          "Cannot do a CFD with a EnumEventDescriptorV0TLV")
      case decomp: DigitDecompositionEventDescriptorV0TLV =>
        require(validateMatchingOracleDescriptors(oracles),
                "Oracles must use same numDigits and base")

        val numDigits = decomp.numDigits.toInt

        val curve = if (isCall) {
          val pointA = OutcomePayoutEndpoint(
            0L,
            individualCollateral.satoshis - premium.satoshis)

          val pointB = OutcomePayoutEndpoint(
            strikePrice,
            individualCollateral.satoshis - premium.satoshis)

          val pointC =
            OutcomePayoutEndpoint(decomp.maxNum.toLong, totalCollateral)
          DLCPayoutCurve(Vector(pointA, pointB, pointC))
        } else {
          val pointA = OutcomePayoutEndpoint(0L, totalCollateral)

          val pointB = OutcomePayoutEndpoint(
            strikePrice,
            individualCollateral.satoshis - premium.satoshis)

          val pointC =
            OutcomePayoutEndpoint(
              decomp.maxNum.toLong,
              individualCollateral.satoshis - premium.satoshis)
          DLCPayoutCurve(Vector(pointA, pointB, pointC))
        }

        NumericContractDescriptor(curve,
                                  numDigits = numDigits,
                                  roundingIntervals)
    }
  }

  override val toContractInfo: ContractInfo = {
    val pair: NumericPair =
      ContractOraclePair.NumericPair(contractDescriptor, oracleInfo)
    ContractInfo(totalCollateral.satoshis, pair)
  }
}

/** @inheritdoc */
case class SingleOracleCallOption(
    individualCollateral: CurrencyUnit,
    totalCollateral: CurrencyUnit,
    oracle: OracleAnnouncementTLV,
    strikePrice: Long,
    premium: CurrencyUnit,
    roundingIntervals: RoundingIntervals
) extends SingleOracleDLCTemplate
    with OptionTemplate {

  override def isCall: Boolean = true

  override val oracleInfo: NumericSingleOracleInfo = NumericSingleOracleInfo(
    oracle)
}

/** @inheritdoc */
case class MultiOracleCallOption(
    individualCollateral: CurrencyUnit,
    totalCollateral: CurrencyUnit,
    oracles: Vector[OracleAnnouncementTLV],
    oracleThreshold: Int,
    strikePrice: Long,
    premium: CurrencyUnit,
    maxErrorExp: Int,
    minFailExp: Int,
    maximizeCoverage: Boolean,
    roundingIntervals: RoundingIntervals
) extends MultiOracleDLCTemplate
    with OptionTemplate {

  require(oracles.nonEmpty, "Cannot have no oracles")
  require(oracleThreshold > 0, "Oracle threshold must be greater than 0")
  require(
    oracles.size >= oracleThreshold,
    s"Oracle threshold cannot be greater than the number of oracles, got ${oracles.size} >= $oracleThreshold")

  override def isCall: Boolean = true

  override val oracleInfo: NumericMultiOracleInfo =
    NumericMultiOracleInfo(threshold = oracleThreshold,
                           announcements = oracles,
                           maxErrorExp = maxErrorExp,
                           minFailExp = minFailExp,
                           maximizeCoverage = maximizeCoverage)
}

/** @inheritdoc */
case class SingleOraclePutOption(
    individualCollateral: CurrencyUnit,
    totalCollateral: CurrencyUnit,
    oracle: OracleAnnouncementTLV,
    strikePrice: Long,
    premium: CurrencyUnit,
    roundingIntervals: RoundingIntervals
) extends SingleOracleDLCTemplate
    with OptionTemplate {

  override def isCall: Boolean = false

  override val oracleInfo: NumericSingleOracleInfo = NumericSingleOracleInfo(
    oracle)
}

/** @inheritdoc */
case class MultiOraclePutOption(
    individualCollateral: CurrencyUnit,
    totalCollateral: CurrencyUnit,
    oracles: Vector[OracleAnnouncementTLV],
    oracleThreshold: Int,
    strikePrice: Long,
    premium: CurrencyUnit,
    maxErrorExp: Int,
    minFailExp: Int,
    maximizeCoverage: Boolean,
    roundingIntervals: RoundingIntervals
) extends MultiOracleDLCTemplate
    with OptionTemplate {

  require(oracles.nonEmpty, "Cannot have no oracles")
  require(oracleThreshold > 0, "Oracle threshold must be greater than 0")
  require(
    oracles.size >= oracleThreshold,
    s"Oracle threshold cannot be greater than the number of oracles, got ${oracles.size} >= $oracleThreshold")

  override def isCall: Boolean = false

  override val oracleInfo: NumericMultiOracleInfo =
    NumericMultiOracleInfo(threshold = oracleThreshold,
                           announcements = oracles,
                           maxErrorExp = maxErrorExp,
                           minFailExp = minFailExp,
                           maximizeCoverage = maximizeCoverage)
}

object DLCTemplate {

  /** Verifies that the oracles are using compatible event descriptors */
  private[dlc] def validateMatchingOracleDescriptors(
      oracles: Vector[OracleAnnouncementTLV]
  ): Boolean = {
    oracles.head.eventTLV.eventDescriptor match {
      case EnumEventDescriptorV0TLV(outcomes) =>
        oracles.forall {
          _.eventTLV.eventDescriptor match {
            case enum: EnumEventDescriptorV0TLV =>
              enum.outcomes.sortBy(_.normStr) == outcomes.sortBy(_.normStr)
            case _: DigitDecompositionEventDescriptorV0TLV => false
          }
        }
      case decomp: DigitDecompositionEventDescriptorV0TLV =>
        oracles.forall {
          _.eventTLV.eventDescriptor match {
            case _: EnumEventDescriptorV0TLV => false
            case d: DigitDecompositionEventDescriptorV0TLV =>
              decomp.numDigits == d.numDigits && decomp.base == d.base
          }
        }
    }
  }
}
