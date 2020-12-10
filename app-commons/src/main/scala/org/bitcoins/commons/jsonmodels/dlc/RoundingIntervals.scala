package org.bitcoins.commons.jsonmodels.dlc

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.util.NumberUtil

import scala.annotation.tailrec

/** Specifies a list of intervals with corresponding rounding moduli.
  * In particular, each element (outcome, roundingMod) of intervalStarts
  * represents the beginning of a new interval at outcome with new modulus roundingMod.
  *
  * @see https://github.com/discreetlogcontracts/dlcspecs/blob/8ee4bbe816c9881c832b1ce320b9f14c72e3506f/NumericOutcome.md#rounding-intervals
  */
case class RoundingIntervals(intervalStarts: Vector[(BigDecimal, Long)]) {
  if (intervalStarts.nonEmpty) {
    require(intervalStarts.init.zip(intervalStarts.tail).forall {
              case (i1, i2) => i1._1 < i2._1
            },
            s"Intervals must be ascending: $intervalStarts")
  }

  /** Returns the rounding interval (start, end, mod) containing the given outcome */
  def intervalContaining(
      outcome: BigDecimal): (BigDecimal, BigDecimal, Long) = {
    // Using Long.MaxValue guarantees that index will point to index of right endpoint of interval
    val index = NumberUtil.search(intervalStarts, (outcome, Long.MaxValue)) - 1

    if (index == -1) {
      val firstIntervalChange =
        intervalStarts.map(_._1).headOption.getOrElse(BigDecimal(Long.MaxValue))
      (Long.MinValue, firstIntervalChange, 1L)
    } else if (index == intervalStarts.length - 1) {
      val (intervalStart, roundingModulus) = intervalStarts.last

      (intervalStart, Long.MaxValue, roundingModulus)
    } else {
      val (intervalStart, roundingModulus) = intervalStarts(index)
      val (intervalEnd, _) = intervalStarts(index + 1)

      (intervalStart, intervalEnd, roundingModulus)
    }
  }

  /** Returns the rounding modulus which should be used at the given outcome */
  def roundingModulusAt(outcome: BigDecimal): Long = {
    intervalContaining(outcome)._3
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

    val builder = Vector.newBuilder[(BigDecimal, Long)]

    @tailrec
    def minMerge(
        thisIntervals: Vector[(BigDecimal, Long)],
        thisCurrentMod: Long,
        otherIntervals: Vector[(BigDecimal, Long)],
        otherCurrentMod: Long): Unit = {
      if (thisIntervals.isEmpty) {
        val otherEnd = otherIntervals.map {
          case (startRange, otherMod) =>
            (startRange, Math.min(thisCurrentMod, otherMod))
        }
        builder.++=(otherEnd)
      } else if (otherIntervals.isEmpty) {
        val thisEnd = thisIntervals.map {
          case (startRange, thisMod) =>
            (startRange, Math.min(thisMod, otherCurrentMod))
        }
        builder.++=(thisEnd)
      } else {
        val (thisNextStart, thisNextMod) = thisIntervals.head
        val (otherNextStart, otherNextMod) = otherIntervals.head

        if (thisNextStart < otherNextStart) {
          builder.+=((thisNextStart, Math.min(thisNextMod, otherCurrentMod)))
          minMerge(thisIntervals.tail,
                   thisNextMod,
                   otherIntervals,
                   otherCurrentMod)
        } else if (thisNextStart > otherNextStart) {
          builder.+=((otherNextStart, Math.min(otherNextMod, thisCurrentMod)))
          minMerge(thisIntervals,
                   thisCurrentMod,
                   otherIntervals.tail,
                   otherNextMod)
        } else {
          builder.+=((thisNextStart, Math.min(thisNextMod, otherNextMod)))
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
      case (_, newMod) =>
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
}
