package org.bitcoins.core.protocol.dlc

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.dlc.RoundingIntervals.{
  Interval,
  IntervalStart
}
import org.bitcoins.core.util.NumberUtil

import scala.annotation.tailrec

/** Specifies a list of intervals with corresponding rounding moduli.
  * In particular, each element (outcome, roundingMod) of intervalStarts
  * represents the beginning of a new interval at outcome with new modulus roundingMod.
  *
  * @see https://github.com/discreetlogcontracts/dlcspecs/blob/8ee4bbe816c9881c832b1ce320b9f14c72e3506f/NumericOutcome.md#rounding-intervals
  */
case class RoundingIntervals(intervalStarts: Vector[IntervalStart]) {
  if (intervalStarts.nonEmpty) {
    require(intervalStarts.init.zip(intervalStarts.tail).forall {
              case (i1, i2) => i1.firstOutcome < i2.firstOutcome
            },
            s"Intervals must be ascending: $intervalStarts")
  }

  /** Returns the rounding interval (start, end, mod) containing the given outcome */
  def intervalContaining(outcome: BigDecimal): Interval = {
    implicit val ord: Ordering[IntervalStart] =
      Ordering.by[IntervalStart, (BigDecimal, Long)](i =>
        (i.firstOutcome, i.roundingMod))

    // Using Long.MaxValue guarantees that index will point to index of right endpoint of interval
    val index =
      NumberUtil.search(intervalStarts,
                        IntervalStart(outcome, Long.MaxValue)) - 1

    if (index == -1) {
      val firstIntervalChange =
        intervalStarts
          .map(_.firstOutcome)
          .headOption
          .getOrElse(BigDecimal(Long.MaxValue))
      Interval(Long.MinValue, firstIntervalChange, 1L)
    } else if (index == intervalStarts.length - 1) {
      val IntervalStart(intervalStart, roundingModulus) = intervalStarts.last

      Interval(intervalStart, Long.MaxValue, roundingModulus)
    } else {
      val IntervalStart(intervalStart, roundingModulus) = intervalStarts(index)
      val IntervalStart(intervalEnd, _) = intervalStarts(index + 1)

      Interval(intervalStart, intervalEnd, roundingModulus)
    }
  }

  /** Returns the rounding modulus which should be used at the given outcome */
  def roundingModulusAt(outcome: BigDecimal): Long = {
    intervalContaining(outcome).roundingMod
  }

  def round(outcome: BigDecimal, computedPayout: Satoshis): Satoshis = {
    val payoutLong = computedPayout.toLong
    val roundingMod = roundingModulusAt(outcome)
    val mod = if (payoutLong >= 0) {
      payoutLong % roundingMod
    } else {
      // (negative number) % _ returns the negative modulus
      payoutLong % roundingMod + roundingMod
    }

    val roundedPayout = if (mod >= roundingMod / 2.0) {
      payoutLong + roundingMod - mod
    } else {
      payoutLong - mod
    }

    Satoshis(roundedPayout)
  }

  /** Returns a RoundingIntervals which does the maximum amount of rounding
    * allowed by both this and other.
    */
  def minRoundingWith(other: RoundingIntervals): RoundingIntervals = {

    val builder = Vector.newBuilder[IntervalStart]

    def addInterval(firstOutcome: BigDecimal, roundingMod: Long): Unit = {
      builder.+=(IntervalStart(firstOutcome, roundingMod))
    }

    @tailrec
    def minMerge(
        thisIntervals: Vector[IntervalStart],
        thisCurrentMod: Long,
        otherIntervals: Vector[IntervalStart],
        otherCurrentMod: Long): Unit = {
      if (thisIntervals.isEmpty) {
        val otherEnd = otherIntervals.map {
          case IntervalStart(startRange, otherMod) =>
            IntervalStart(startRange, Math.min(thisCurrentMod, otherMod))
        }
        builder.++=(otherEnd)
      } else if (otherIntervals.isEmpty) {
        val thisEnd = thisIntervals.map {
          case IntervalStart(startRange, thisMod) =>
            IntervalStart(startRange, Math.min(thisMod, otherCurrentMod))
        }
        builder.++=(thisEnd)
      } else {
        val IntervalStart(thisNextStart, thisNextMod) = thisIntervals.head
        val IntervalStart(otherNextStart, otherNextMod) = otherIntervals.head

        if (thisNextStart < otherNextStart) {
          addInterval(thisNextStart, Math.min(thisNextMod, otherCurrentMod))
          minMerge(thisIntervals.tail,
                   thisNextMod,
                   otherIntervals,
                   otherCurrentMod)
        } else if (thisNextStart > otherNextStart) {
          addInterval(otherNextStart, Math.min(otherNextMod, thisCurrentMod))
          minMerge(thisIntervals,
                   thisCurrentMod,
                   otherIntervals.tail,
                   otherNextMod)
        } else {
          addInterval(thisNextStart, Math.min(thisNextMod, otherNextMod))
          minMerge(thisIntervals.tail,
                   thisNextMod,
                   otherIntervals.tail,
                   otherNextMod)
        }
      }
    }

    minMerge(thisIntervals = intervalStarts,
             thisCurrentMod = 1L,
             otherIntervals = other.intervalStarts,
             otherCurrentMod = 1L)

    RoundingIntervals(builder.result()).canonicalForm()
  }

  def canonicalForm(): RoundingIntervals = {
    var currentMod: Long = 1L

    val canonicalVec = intervalStarts.filter {
      case IntervalStart(_, newMod) =>
        if (newMod == currentMod) false
        else {
          currentMod = newMod
          true
        }
    }

    RoundingIntervals(canonicalVec)
  }
}

object RoundingIntervals {
  val noRounding: RoundingIntervals = RoundingIntervals(Vector.empty)

  case class IntervalStart(firstOutcome: BigDecimal, roundingMod: Long)

  case class Interval(
      firstOutcome: BigDecimal,
      nextFirstOutcome: BigDecimal,
      roundingMod: Long) {
    require(firstOutcome < nextFirstOutcome,
            s"First outcome must come before last, $this")
  }
}
