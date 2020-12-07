package org.bitcoins.gui.dlc

import breeze.plot.{plot, Figure}
import org.bitcoins.commons.jsonmodels.dlc.{
  CETCalculator,
  OutcomeValueFunction,
  RoundingIntervals
}
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.util.NumberUtil

object DLCPlotUtil {

  def plotCETsWithOriginalCurve(
      base: Int,
      numDigits: Int,
      function: OutcomeValueFunction,
      totalCollateral: Satoshis,
      rounding: RoundingIntervals): Figure = {
    plotCETsWithOriginalCurve(base,
                              numDigits,
                              function,
                              totalCollateral,
                              rounding,
                              executedCETOpt = None)
  }

  def plotCETsWithOriginalCurve(
      base: Int,
      numDigits: Int,
      function: OutcomeValueFunction,
      totalCollateral: Satoshis,
      rounding: RoundingIntervals,
      executedCET: Vector[Int]): Figure = {
    plotCETsWithOriginalCurve(base,
                              numDigits,
                              function,
                              totalCollateral,
                              rounding,
                              executedCETOpt = Some(executedCET))
  }

  private def plotCETsWithOriginalCurve(
      base: Int,
      numDigits: Int,
      function: OutcomeValueFunction,
      totalCollateral: Satoshis,
      rounding: RoundingIntervals,
      executedCETOpt: Option[Vector[Int]]): Figure = {
    val xs = 0.until(Math.pow(base, numDigits).toInt - 1).toVector
    val ys = xs.map(function.apply(_).toLong.toInt)

    val figure = plotCETs(base,
                          numDigits,
                          function,
                          totalCollateral,
                          rounding,
                          executedCETOpt)
    figure.subplot(0) += plot(xs, ys, name = "Original Curve")
    figure
  }

  def plotCETs(
      base: Int,
      numDigits: Int,
      function: OutcomeValueFunction,
      totalCollateral: Satoshis,
      rounding: RoundingIntervals): Figure = {
    plotCETs(base,
             numDigits,
             function,
             totalCollateral,
             rounding,
             executedCETOpt = None)
  }

  def plotCETs(
      base: Int,
      numDigits: Int,
      function: OutcomeValueFunction,
      totalCollateral: Satoshis,
      rounding: RoundingIntervals,
      executedDLC: Vector[Int]): Figure = {
    plotCETs(base,
             numDigits,
             function,
             totalCollateral,
             rounding,
             executedCETOpt = Some(executedDLC))
  }

  private def plotCETs(
      base: Int,
      numDigits: Int,
      function: OutcomeValueFunction,
      totalCollateral: Satoshis,
      rounding: RoundingIntervals,
      executedCETOpt: Option[Vector[Int]]): Figure = {
    val cets = CETCalculator.computeCETs(base,
                                         numDigits,
                                         function,
                                         totalCollateral,
                                         rounding)

    plotCETs(cets, base, numDigits, executedCETOpt)
  }

  def plotCETs(
      cets: Vector[(Vector[Int], Satoshis)],
      base: Int,
      numDigits: Int): Figure = {
    plotCETs(cets, base, numDigits, executedCETOpt = None)
  }

  def plotCETs(
      cets: Vector[(Vector[Int], Satoshis)],
      base: Int,
      numDigits: Int,
      executedCET: Vector[Int]): Figure = {
    plotCETs(cets, base, numDigits, executedCETOpt = Some(executedCET))
  }

  private def plotCETs(
      cets: Vector[(Vector[Int], Satoshis)],
      base: Int,
      numDigits: Int,
      executedCETOpt: Option[Vector[Int]]): Figure = {
    def fromDigits(digits: Vector[Int]): Int = {
      NumberUtil.fromDigits(digits, base, numDigits).toInt
    }

    val xs = cets.map(_._1).map(fromDigits)
    val ys = cets.map(_._2.toLong.toInt)

    val figure = Figure("DLC Payout Curve")
    val cetPlot = figure.subplot(0)

    val canonicalCETOpt = executedCETOpt
      .flatMap { outcome =>
        CETCalculator.searchForPrefix(outcome, cets.map(_._1))(identity)
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
                    name = s"CETs (${cets.length})",
                    labels = labels)
    cetPlot.xlabel = "Outcome"
    cetPlot.ylabel = "Payout (sats)"
    cetPlot.legend = true

    figure
  }
}
