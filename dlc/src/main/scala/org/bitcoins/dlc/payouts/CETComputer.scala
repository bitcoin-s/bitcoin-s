package org.bitcoins.dlc.payouts

import org.bitcoins.core.currency.Satoshis

object CETComputer {

  // Inclusive
  sealed trait CETRange {
    def indexFrom: Long
    def indexTo: Long
  }
  case class StartZero(indexFrom: Long, indexTo: Long) extends CETRange
  case class StartTotal(indexFrom: Long, indexTo: Long) extends CETRange
  case class StartFunc(indexFrom: Long, indexTo: Long) extends CETRange

  object CETRange {

    def apply(
        index: Long,
        value: Satoshis,
        totalCollateral: Satoshis): CETRange = {
      if (value <= Satoshis.zero) {
        StartZero(index, index)
      } else if (value >= totalCollateral) {
        StartTotal(index, index)
      } else {
        StartFunc(index, index)
      }
    }
  }

  def splitIntoRanges(
      from: Long,
      to: Long,
      totalCollateral: Satoshis,
      function: OutcomeValueFunction): Vector[CETRange] = {
    val rangeBuilder = Vector.newBuilder[CETRange]
    var currentRange: CETRange =
      CETRange(from, function(from), totalCollateral)

    def newRange(num: Long, value: Satoshis): Unit = {
      rangeBuilder += currentRange
      currentRange = CETRange(num, value, totalCollateral)
    }

    from.to(to).foreach { num =>
      val value = function(num)
      if (value <= Satoshis.zero) {
        currentRange match {
          case StartZero(indexFrom, _) =>
            currentRange = StartZero(indexFrom, num)
          case _: StartTotal | _: StartFunc => newRange(num, value)
        }
      } else if (value >= totalCollateral) {
        currentRange match {
          case StartTotal(indexFrom, _) =>
            currentRange = StartTotal(indexFrom, num)
          case _: StartZero | _: StartFunc => newRange(num, value)
        }
      } else {
        currentRange match {
          case StartFunc(indexFrom, _) =>
            currentRange = StartFunc(indexFrom, num)
          case _: StartZero | _: StartTotal => newRange(num, value)
        }
      }
    }

    rangeBuilder += currentRange

    rangeBuilder.result()
  }

  // From 0th place to numDigits place
  def decompose(num: Long, base: Int, numDigits: Int): Vector[Int] = {
    var currentNum: Long = num

    (0 until numDigits).toVector.map { _ =>
      val digit = currentNum % base
      currentNum = currentNum / base

      digit.toInt
    }
  }

  def frontGroupingsWithFixedDigit(
      uniqueStartDigits: Vector[Int],
      base: Int): Vector[Vector[Int]] = {
    if (uniqueStartDigits.init.forall(_ == 0)) {
      Vector(Vector(uniqueStartDigits.last))
    } else {
      uniqueStartDigits.reverse +: uniqueStartDigits.zipWithIndex.init.flatMap {
        case (lastImportantDigit, unimportantDigits) =>
          val fixedDigits =
            uniqueStartDigits.drop(unimportantDigits + 1).reverse
          (lastImportantDigit + 1).until(base).map { lastDigit =>
            fixedDigits :+ lastDigit
          }
      }
    }
  }

  def backGroupingsWithFixedDigit(
      uniqueEndDigits: Vector[Int],
      base: Int): Vector[Vector[Int]] = {
    if (uniqueEndDigits.init.forall(_ == base - 1)) {
      Vector(Vector(uniqueEndDigits.last))
    } else {
      val fromBack = uniqueEndDigits.zipWithIndex.init.flatMap {
        case (lastImportantDigit, unimportantDigits) =>
          val fixedDigits = uniqueEndDigits.drop(unimportantDigits + 1).reverse
          0.until(lastImportantDigit).reverseIterator.toVector.map {
            lastDigit =>
              fixedDigits :+ lastDigit
          }
      }

      fromBack.reverse :+ uniqueEndDigits.reverse
    }
  }

  def firstDigitFixedGroupings(
      firstDigitLower: Int,
      firstDigitUpper: Int): Vector[Vector[Int]] = {
    (firstDigitLower + 1).until(firstDigitUpper).toVector.map { firstDigit =>
      Vector(firstDigit)
    }
  }

  def groupByIgnoringDigits(
      startIndex: Long,
      endIndex: Long,
      base: Int,
      numDigits: Int): Vector[Vector[Int]] = {
    val startDigits = decompose(startIndex, base, numDigits)
    val endDigits = decompose(endIndex, base, numDigits)

    val sharedDigits = startDigits
      .zip(endDigits)
      .reverse
      .takeWhile { case (startDigit, endDigit) => startDigit == endDigit }
      .map(_._1)

    val uniqueStartDigits = startDigits.dropRight(sharedDigits.length)
    val uniqueEndDigits = endDigits.dropRight(sharedDigits.length)

    if (sharedDigits.length == startDigits.length) {
      Vector.empty
    } else if (sharedDigits.length == startDigits.length - 1) {
      startDigits.head.to(endDigits.head).toVector.map { lastDigit =>
        startDigits.tail.reverse :+ lastDigit
      }
    } else {
      val frontGroupings = frontGroupingsWithFixedDigit(uniqueStartDigits, base)
      val firstDigitGroupings =
        firstDigitFixedGroupings(uniqueStartDigits.last, uniqueEndDigits.last)
      val backGroupings = backGroupingsWithFixedDigit(uniqueEndDigits, base)

      val uniqueDigitsGroupings =
        frontGroupings ++ firstDigitGroupings ++ backGroupings

      uniqueDigitsGroupings.map { uniqueDigits =>
        sharedDigits ++ uniqueDigits
      }
    }
  }

  def computeCETs(
      base: Int,
      numDigits: Int,
      function: OutcomeValueFunction,
      totalCollateral: Satoshis): Vector[(Vector[Int], Satoshis)] = {
    val min = 0
    val max = Math.pow(base, numDigits).toLong - 1

    val ranges = splitIntoRanges(min, max, totalCollateral, function)

    ranges.flatMap { range =>
      range match {
        case StartZero(indexFrom, indexTo) =>
          groupByIgnoringDigits(indexFrom, indexTo, base, numDigits).map(
            _ -> Satoshis.zero)
        case StartTotal(indexFrom, indexTo) =>
          groupByIgnoringDigits(indexFrom, indexTo, base, numDigits).map(
            _ -> totalCollateral)
        case StartFunc(indexFrom, indexTo) =>
          indexFrom.to(indexTo).map { num =>
            decompose(num, base, numDigits).reverse -> function(num)
          }
      }
    }
  }
}
