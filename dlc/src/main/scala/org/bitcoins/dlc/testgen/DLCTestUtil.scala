package org.bitcoins.dlc.testgen

import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.protocol.dlc.DLCMessage.{
  EnumContractDescriptor,
  NumericContractDescriptor
}
import org.bitcoins.core.protocol.dlc.{
  DLCPayoutCurve,
  OutcomePayoutEndpoint,
  RoundingIntervals
}
import org.bitcoins.core.protocol.tlv.EnumOutcome
import org.bitcoins.core.util.NumberUtil

object DLCTestUtil {

  def genOutcomes(size: Int): Vector[String] = {
    (0 until size).map(_ => scala.util.Random.nextLong().toString).toVector
  }

  def genValues(size: Int, totalAmount: CurrencyUnit): Vector[Satoshis] = {
    val vals = if (size < 2) {
      throw new IllegalArgumentException(
        s"Size must be at least two, got $size")
    } else if (size == 2) {
      Vector(totalAmount.satoshis, Satoshis.zero)
    } else {
      (0 until size - 2).map { _ =>
        Satoshis(NumberUtil.randomLong(totalAmount.satoshis.toLong))
      }.toVector :+ totalAmount.satoshis :+ Satoshis.zero
    }

    val valsWithOrder = vals.map(_ -> scala.util.Random.nextDouble())
    valsWithOrder.sortBy(_._2).map(_._1)
  }

  def genContractDescriptors(
      outcomes: Vector[String],
      totalInput: CurrencyUnit): (
      EnumContractDescriptor,
      EnumContractDescriptor) = {
    val outcomeMap =
      outcomes
        .map(EnumOutcome.apply)
        .zip(DLCTestUtil.genValues(outcomes.length, totalInput))

    val info = EnumContractDescriptor(outcomeMap)
    val remoteInfo = info.flip(totalInput.satoshis)

    (info, remoteInfo)
  }

  /** Generates a collared forward contract.
    *
    * If roundingIntervals is noRounding and numRounds > 0, then
    * roundingIntervals is ignored and instead the contract is rounded
    * in numRounds different ways in between the collars.
    * Otherwise roundingIntervals is used.
    */
  def genMultiDigitContractInfo(
      numDigits: Int,
      totalCollateral: CurrencyUnit,
      roundingIntervals: RoundingIntervals = RoundingIntervals.noRounding,
      numRounds: Int = 0): (
      NumericContractDescriptor,
      NumericContractDescriptor) = {
    val overMaxValue = Math.pow(2, numDigits).toLong
    // Left collar goes from [0, botCollar]
    val botCollar = NumberUtil.randomLong(overMaxValue / 2)
    val halfWindow = scala.math.min(overMaxValue / 4, 2500)
    val topCollarDiff = NumberUtil.randomLong(halfWindow)
    // Right collar goes from [topCollar, overMaxValue)
    val topCollar = botCollar + halfWindow + topCollarDiff
    val isGoingLong = scala.util.Random.nextBoolean()
    // leftVal and rightVal determine whether the contract shape
    // goes from total to 0 or 0 to total
    val (leftVal, rightVal) =
      if (isGoingLong) (Satoshis.zero, totalCollateral.satoshis)
      else (totalCollateral.satoshis, Satoshis.zero)
    val func = DLCPayoutCurve(
      Vector(
        OutcomePayoutEndpoint(0, leftVal),
        OutcomePayoutEndpoint(botCollar + 1, leftVal),
        OutcomePayoutEndpoint(topCollar, rightVal),
        OutcomePayoutEndpoint(overMaxValue - 1, rightVal)
      ))
    val roundingIntervalsToUse =
      if (numRounds > 0 && roundingIntervals == RoundingIntervals.noRounding) {
        val intervalStarts = 0.until(numRounds).toVector.map { num =>
          val intervalStart =
            ((numRounds - num) * botCollar + num * topCollar) / numRounds
          val roundingMod = 1L << num
          RoundingIntervals.IntervalStart(BigDecimal(intervalStart),
                                          roundingMod)
        }
        RoundingIntervals(intervalStarts)
      } else roundingIntervals
    val info =
      NumericContractDescriptor(func, numDigits, roundingIntervalsToUse)
    val remoteInfo = info.flip(totalCollateral.satoshis)
    (info, remoteInfo)
  }
}
