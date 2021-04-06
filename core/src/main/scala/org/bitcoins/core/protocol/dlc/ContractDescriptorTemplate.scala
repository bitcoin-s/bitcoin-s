package org.bitcoins.core.protocol.dlc

import org.bitcoins.core.currency._

sealed trait ContractDescriptorTemplate {
  def individualCollateral: CurrencyUnit

  def totalCollateral: CurrencyUnit

  def toContractDescriptor: ContractDescriptor

  require(
    individualCollateral >= Satoshis.zero,
    s"individualCollateral must be greater than or equal to zero, got $individualCollateral")

  require(
    totalCollateral > Satoshis.zero,
    s"totalCollateral must be greater than or equal to zero, got $totalCollateral")

  require(
    individualCollateral <= totalCollateral,
    s"Cannot have individual collateral ($individualCollateral) be greater than totalCollateral ($totalCollateral)"
  )
}

/** A template for creating a Contract For Difference DLC
  *
  * @see https://www.investopedia.com/terms/c/contractfordifferences.asp
  */
sealed trait CFDTemplate extends ContractDescriptorTemplate {

  def strikePrice: Long

  def roundingIntervals: RoundingIntervals

  def numDigits: Int

  require(numDigits > 0,
          s"Num digits must be greater than zero, got $numDigits")
  require(
    strikePrice >= 0,
    s"Strike price must be greater than or equal to zero, got $strikePrice")

  override val toContractDescriptor: NumericContractDescriptor = {
    val func: Long => Long = { outcome =>
      if (outcome == 0) Long.MaxValue
      else (strikePrice * individualCollateral.satoshis.toLong) / outcome
    }

    // TODO use hyperbola DLC
    val curve = CETCalculator.lineApprox(func, numDigits, 100)

    val descriptor =
      NumericContractDescriptor(curve, numDigits = numDigits, roundingIntervals)

    this match {
      case _: ShortCFD => descriptor
      case _: LongCFD  => descriptor.flip(totalCollateral.satoshis)
    }
  }
}

/** @inheritdoc */
case class LongCFD(
    individualCollateral: CurrencyUnit,
    totalCollateral: CurrencyUnit,
    numDigits: Int,
    strikePrice: Long,
    roundingIntervals: RoundingIntervals
) extends CFDTemplate

/** @inheritdoc */
case class ShortCFD(
    individualCollateral: CurrencyUnit,
    totalCollateral: CurrencyUnit,
    numDigits: Int,
    strikePrice: Long,
    roundingIntervals: RoundingIntervals
) extends CFDTemplate

/** A template for doing an options contract DLC
  *
  * @see https://www.investopedia.com/terms/o/optionscontract.asp
  */
sealed trait OptionTemplate extends ContractDescriptorTemplate {
  def premium: CurrencyUnit

  def strikePrice: Long

  def numDigits: Int

  def roundingIntervals: RoundingIntervals

  require(premium >= Satoshis.zero,
          s"Premium must be greater than or equal to zero, got $premium")

  require(numDigits > 0,
          s"Num digits must be greater than zero, got $numDigits")
  require(
    strikePrice >= 0,
    s"Strike price must be greater than or equal to zero, got $strikePrice")

  override val toContractDescriptor: NumericContractDescriptor = {
    val maxNum: Long = (BigInt(2).pow(numDigits) - 1).toLong

    val curve = this match {
      case _: CallOption =>
        val pointA = OutcomePayoutEndpoint(
          0L,
          individualCollateral.satoshis - premium.satoshis)

        val pointB = OutcomePayoutEndpoint(
          strikePrice,
          individualCollateral.satoshis - premium.satoshis)

        val pointC =
          OutcomePayoutEndpoint(maxNum, totalCollateral)
        DLCPayoutCurve(Vector(pointA, pointB, pointC))
      case _: PutOption =>
        val pointA = OutcomePayoutEndpoint(0L, totalCollateral)

        val pointB = OutcomePayoutEndpoint(
          strikePrice,
          individualCollateral.satoshis - premium.satoshis)

        val pointC =
          OutcomePayoutEndpoint(
            maxNum,
            individualCollateral.satoshis - premium.satoshis)
        DLCPayoutCurve(Vector(pointA, pointB, pointC))
    }

    NumericContractDescriptor(curve, numDigits = numDigits, roundingIntervals)
  }
}

/** @inheritdoc */
case class CallOption(
    individualCollateral: CurrencyUnit,
    totalCollateral: CurrencyUnit,
    numDigits: Int,
    strikePrice: Long,
    premium: CurrencyUnit,
    roundingIntervals: RoundingIntervals
) extends OptionTemplate

/** @inheritdoc */
case class PutOption(
    individualCollateral: CurrencyUnit,
    totalCollateral: CurrencyUnit,
    numDigits: Int,
    strikePrice: Long,
    premium: CurrencyUnit,
    roundingIntervals: RoundingIntervals
) extends OptionTemplate
