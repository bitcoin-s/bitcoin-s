package org.bitcoins.commons.jsonmodels.dlc

import org.bitcoins.core.currency.Satoshis

import scala.annotation.tailrec

case class RoundingIntervals(intervalStarts: Vector[(BigDecimal, Long)]) {

  def intervalContaining(
      outcome: BigDecimal): (BigDecimal, BigDecimal, Long) = {
    // Using Long.MaxValue guarantees that index will point to index of right endpoint of interval
    val index =
      intervalStarts.search((outcome, Long.MaxValue)).insertionPoint - 1

    if (index == -1) {
      (Long.MinValue, intervalStarts.head._1, 1L)
    } else if (index == intervalStarts.length - 1) {
      val (intervalStart, roundingModulus) = intervalStarts.last

      (intervalStart, Long.MaxValue, roundingModulus)
    } else {
      val (intervalStart, roundingModulus) = intervalStarts(index)
      val (intervalEnd, _) = intervalStarts(index + 1)

      (intervalStart, intervalEnd, roundingModulus)
    }
  }

  def roundingModulusAt(outcome: BigDecimal): Long = {
    // Using Long.MaxValue guarantees that index will point to index of right endpoint of interval
    val index =
      intervalStarts.search((outcome, Long.MaxValue)).insertionPoint - 1

    if (index == -1) 1L
    else intervalStarts(index)._2
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
