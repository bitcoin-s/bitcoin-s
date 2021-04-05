package org.bitcoins.core.protocol.dlc

import org.bitcoins.core.currency._

sealed trait ContractDescriptorTemplate {
  def individualCollateral: CurrencyUnit

  def totalCollateral: CurrencyUnit

  def toContractDescriptor: ContractDescriptor
}

/** A template for creating a Contract For Difference DLC
  *
  * @see https://www.investopedia.com/terms/c/contractfordifferences.asp
  */
sealed trait CFDTemplate extends ContractDescriptorTemplate {

  def strikePrice: Long

  def roundingIntervals: RoundingIntervals

  def isLong: Boolean

  def numDigits: Int

  override val toContractDescriptor: NumericContractDescriptor = {
    val func: Long => Long = { outcome =>
      if (outcome == 0) Long.MaxValue
      else (strikePrice * individualCollateral.satoshis.toLong) / outcome
    }

    // TODO use hyperbola DLC
    val curve = CETCalculator.lineApprox(func, numDigits, 100)

    val descriptor =
      NumericContractDescriptor(curve, numDigits = numDigits, roundingIntervals)

    if (isLong) {
      descriptor.flip(totalCollateral.satoshis)
    } else descriptor
  }
}

/** @inheritdoc */
case class LongCFD(
    individualCollateral: CurrencyUnit,
    totalCollateral: CurrencyUnit,
    numDigits: Int,
    strikePrice: Long,
    roundingIntervals: RoundingIntervals
) extends CFDTemplate {
  override val isLong: Boolean = true
}

/** @inheritdoc */
case class ShortCFD(
    individualCollateral: CurrencyUnit,
    totalCollateral: CurrencyUnit,
    numDigits: Int,
    strikePrice: Long,
    roundingIntervals: RoundingIntervals
) extends CFDTemplate {
  override val isLong: Boolean = false
}

/** A template for doing an options contract DLC
  *
  * @see https://www.investopedia.com/terms/o/optionscontract.asp
  */
sealed trait OptionTemplate extends ContractDescriptorTemplate {
  def premium: CurrencyUnit

  def strikePrice: Long

  def isCallOption: Boolean

  def numDigits: Int

  def roundingIntervals: RoundingIntervals

  override val toContractDescriptor: NumericContractDescriptor = {
    val maxNum: Long = (BigInt(2).pow(numDigits) - 1).toLong

    val curve = if (isCallOption) {
      val pointA = OutcomePayoutEndpoint(
        0L,
        individualCollateral.satoshis - premium.satoshis)

      val pointB = OutcomePayoutEndpoint(
        strikePrice,
        individualCollateral.satoshis - premium.satoshis)

      val pointC =
        OutcomePayoutEndpoint(maxNum, totalCollateral)
      DLCPayoutCurve(Vector(pointA, pointB, pointC))
    } else {
      val pointA = OutcomePayoutEndpoint(0L, totalCollateral)

      val pointB = OutcomePayoutEndpoint(
        strikePrice,
        individualCollateral.satoshis - premium.satoshis)

      val pointC =
        OutcomePayoutEndpoint(maxNum,
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
) extends OptionTemplate {

  override val isCallOption: Boolean = true
}

/** @inheritdoc */
case class PutOption(
    individualCollateral: CurrencyUnit,
    totalCollateral: CurrencyUnit,
    numDigits: Int,
    strikePrice: Long,
    premium: CurrencyUnit,
    roundingIntervals: RoundingIntervals
) extends OptionTemplate {

  override val isCallOption: Boolean = false
}
