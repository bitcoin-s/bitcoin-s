package org.bitcoins.dlc.testgen

import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.protocol.dlc.DLCMessage.{
  MultiNonceContractInfo,
  SingleNonceContractInfo
}
import org.bitcoins.core.protocol.dlc.{DLCPayoutCurve, OutcomePayoutEndpoint}
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

  def genContractInfos(outcomes: Vector[String], totalInput: CurrencyUnit): (
      SingleNonceContractInfo,
      SingleNonceContractInfo) = {
    val outcomeMap =
      outcomes
        .map(EnumOutcome.apply)
        .zip(DLCTestUtil.genValues(outcomes.length, totalInput))

    val info = SingleNonceContractInfo(outcomeMap)
    val remoteInfo = info.flip(totalInput.satoshis)

    (info, remoteInfo)
  }

  /** Generates a collared forward contract */
  def genMultiDigitContractInfo(
      numDigits: Int,
      totalCollateral: CurrencyUnit): (
      MultiNonceContractInfo,
      MultiNonceContractInfo) = {
    val overMaxValue = Math.pow(10, numDigits).toLong
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
        OutcomePayoutEndpoint(botCollar, leftVal),
        OutcomePayoutEndpoint(topCollar, rightVal),
        OutcomePayoutEndpoint(overMaxValue - 1, rightVal)
      ))
    val info = MultiNonceContractInfo(func,
                                      base = 10,
                                      numDigits,
                                      totalCollateral.satoshis)
    val remoteInfo = info.flip(totalCollateral.satoshis)
    (info, remoteInfo)
  }
}
