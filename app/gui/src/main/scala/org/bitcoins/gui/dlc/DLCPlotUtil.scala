package org.bitcoins.gui.dlc

import breeze.plot.{plot, Figure}
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.dlc.compute.CETCalculator
import org.bitcoins.core.protocol.dlc.compute.CETCalculator.CETOutcome
import org.bitcoins.core.protocol.dlc.models.{DLCPayoutCurve, RoundingIntervals}
import org.bitcoins.core.util.NumberUtil

object DLCPlotUtil {

  private val CET_COLOR = "65, 105, 225" // royalblue
  private val ORIGINAL_CURVE_COLOR = "50, 205, 50" // limegreen
  private val DUST_COLOR = "255, 140, 0" // dark orange
  private val BREAKEVEN_COLOR = "255, 255, 0" // yellow

  def plotCETsWithOriginalCurve(
      base: Int,
      numDigits: Int,
      function: DLCPayoutCurve,
      totalCollateral: Satoshis,
      rounding: RoundingIntervals): Figure = {
    plotCETsWithOriginalCurve(base,
                              numDigits,
                              function,
                              totalCollateral,
                              rounding,
                              executedCETOpt = None,
                              breakevenOpt = None)
  }

  def plotCETsWithOriginalCurve(
      base: Int,
      numDigits: Int,
      function: DLCPayoutCurve,
      totalCollateral: Satoshis,
      rounding: RoundingIntervals,
      breakevenOpt: Option[Int]): Figure = {
    plotCETsWithOriginalCurve(base,
                              numDigits,
                              function,
                              totalCollateral,
                              rounding,
                              executedCETOpt = None,
                              breakevenOpt)
  }

  def plotCETsWithOriginalCurve(
      base: Int,
      numDigits: Int,
      function: DLCPayoutCurve,
      totalCollateral: Satoshis,
      rounding: RoundingIntervals,
      executedCET: Vector[Int]): Figure = {
    plotCETsWithOriginalCurve(base,
                              numDigits,
                              function,
                              totalCollateral,
                              rounding,
                              executedCETOpt = Some(executedCET),
                              breakevenOpt = None)
  }

  private def plotCETsWithOriginalCurve(
      base: Int,
      numDigits: Int,
      function: DLCPayoutCurve,
      totalCollateral: Satoshis,
      rounding: RoundingIntervals,
      executedCETOpt: Option[Vector[Int]],
      breakevenOpt: Option[Int]): Figure = {
    val xs = 0.until(Math.pow(base, numDigits).toInt - 1).toVector
    val ys = xs.map(function.apply(_).toLong.toInt)

    val figure = plotCETs(base,
                          numDigits,
                          function,
                          totalCollateral,
                          rounding,
                          executedCETOpt,
                          breakevenOpt)
    figure
      .subplot(0) += plot(xs, ys, '-', ORIGINAL_CURVE_COLOR, "Original Curve")
    figure
  }

  def plotCETs(
      base: Int,
      numDigits: Int,
      function: DLCPayoutCurve,
      totalCollateral: Satoshis,
      rounding: RoundingIntervals): Figure = {
    plotCETs(base,
             numDigits,
             function,
             totalCollateral,
             rounding,
             executedCETOpt = None,
             breakevenOpt = None)
  }

  def plotCETs(
      base: Int,
      numDigits: Int,
      function: DLCPayoutCurve,
      totalCollateral: Satoshis,
      rounding: RoundingIntervals,
      executedDLC: Vector[Int]): Figure = {
    plotCETs(base,
             numDigits,
             function,
             totalCollateral,
             rounding,
             executedCETOpt = Some(executedDLC),
             breakevenOpt = None)
  }

  def plotCETs(
      base: Int,
      numDigits: Int,
      function: DLCPayoutCurve,
      totalCollateral: Satoshis,
      rounding: RoundingIntervals,
      executedCETOpt: Option[Vector[Int]],
      breakevenOpt: Option[Int]): Figure = {
    val cets: Vector[CETOutcome] = CETCalculator.computeCETs(base,
                                                             numDigits,
                                                             function,
                                                             totalCollateral,
                                                             rounding)

    plotCETs(cets, base, numDigits, executedCETOpt, breakevenOpt)
  }

  def plotCETs(cets: Vector[CETOutcome], base: Int, numDigits: Int): Figure = {
    plotCETs(cets, base, numDigits, executedCETOpt = None, breakevenOpt = None)
  }

  def plotCETs(
      cets: Vector[CETOutcome],
      base: Int,
      numDigits: Int,
      executedCET: Vector[Int]): Figure = {
    plotCETs(cets,
             base,
             numDigits,
             executedCETOpt = Some(executedCET),
             breakevenOpt = None)
  }

  private def plotCETs(
      cets: Vector[CETOutcome],
      base: Int,
      numDigits: Int,
      executedCETOpt: Option[Vector[Int]],
      breakevenOpt: Option[Int]): Figure = {
    def fromDigits(digits: Vector[Int]): Int = {
      NumberUtil.fromDigits(digits, base, numDigits).toInt
    }

    val xs = cets.map(_.digits).map(fromDigits)
    val ys = cets.map(_.payout.toLong.toInt)

    val figure = Figure("DLC Payout Curve")
    val cetPlot = figure.subplot(0)

    val canonicalCETOpt = executedCETOpt
      .flatMap { outcome =>
        CETCalculator.searchForPrefix(outcome, cets.map(_.digits))(identity)
      }
      .map(fromDigits)
    val markedCETNumOpt = canonicalCETOpt.map(xs.indexOf)
    val labels = { x: Int =>
      if (markedCETNumOpt.contains(x))
        s"Executed CET(${canonicalCETOpt.get}, ${ys(x)})"
      else ""
    }

    cetPlot += plot(xs,
                    ys,
                    '+',
                    CET_COLOR,
                    s"CETs (${cets.length})",
                    labels = labels)
    cetPlot.xlabel = "Outcome"
    cetPlot.ylabel = "Payout (sats)"
    cetPlot.legend = true

    cetPlot += getFlatLineSeries(base,
                                 numDigits,
                                 Policy.dustThreshold.satoshis.toLong.toInt,
                                 '.',
                                 DUST_COLOR,
                                 "Dust Threshold")

    breakevenOpt match {
      case Some(b) =>
        cetPlot += getFlatLineSeries(base,
                                     numDigits,
                                     b,
                                     '.',
                                     BREAKEVEN_COLOR,
                                     "Breakeven")
      case None =>
    }

    figure
  }

  private def getFlatLineSeries(
      base: Int,
      numDigits: Int,
      yValue: Int,
      style: Char,
      colorcode: String,
      name: String) = {
    val maxVal = Math.pow(base, numDigits).toInt
    val segmentLength = math.ceil(maxVal / 100.0).toInt
    val adjustedMaxVal = maxVal + segmentLength
    val xs = 0.until(adjustedMaxVal, segmentLength)
    val ys = xs.map(_ => yValue)
    plot(xs, ys, style, colorcode, name)
  }
}
