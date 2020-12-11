package org.bitcoins.commons.jsonmodels.dlc

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.tlv.{
  DLCOutcomeType,
  EnumOutcome,
  UnsignedNumericOutcome
}
import org.bitcoins.core.util.NumberUtil

import scala.annotation.tailrec

object CETCalculator {

  /** Given a range and a payout function with which to build CETs,
    * the first step is to split the range into sub-ranges which
    * can be compressed or cannot be compressed, represented here
    * as CETRanges.
    *
    * These ranges are inclusive in both indices.
    */
  sealed trait CETRange {
    def indexFrom: Long
    def indexTo: Long
  }

  /** This range contains payouts all <= 0
    * (Note that interpolated functions are allowed
    * to be negative, but we set all negative values to 0).
    */
  case class StartZero(indexFrom: Long, indexTo: Long) extends CETRange

  /** This range contains payouts all >= totalCollateral */
  case class StartTotal(indexFrom: Long, indexTo: Long) extends CETRange

  /** This range contains payouts that all vary at every step and cannot be compressed */
  case class StartFunc(indexFrom: Long, indexTo: Long) extends CETRange

  /** This range contains some constant payout between 0 and totalCollateral (exclusive).
    * To be clear, indexFrom and indexTo are still inclusive values.
    */
  case class StartFuncConst(indexFrom: Long, indexTo: Long) extends CETRange

  object CETRange {

    /** Creates a new CETRange with a single element range */
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

  /** Goes between from and to (inclusive) and evaluates function to split the
    * interval [to, from] into CETRanges.
    */
  def splitIntoRanges(
      from: Long,
      to: Long,
      totalCollateral: Satoshis,
      function: OutcomeValueFunction,
      rounding: RoundingIntervals): Vector[CETRange] = {
    var componentStart = from
    var (currentFunc, componentIndex) = function.componentFor(from)
    var prevFunc = currentFunc

    val rangeBuilder = Vector.newBuilder[CETRange]
    var currentRange: CETRange =
      CETRange(from, currentFunc(from, rounding), totalCollateral)

    var num = from

    def newRange(value: Satoshis): Unit = {
      rangeBuilder += currentRange
      currentRange = CETRange(num, value, totalCollateral)
    }

    def updateComponent(): Unit = {
      componentStart = num
      prevFunc = currentFunc
      componentIndex = componentIndex + 1
      currentFunc = function.functionComponents(componentIndex)
    }

    @tailrec
    def processConstantComponents(): Unit = {
      currentFunc match {
        case OutcomeValueConstant(_, rightEndpoint) =>
          val componentEnd = rightEndpoint.outcome.toLongExact - 1
          val funcValue = rightEndpoint.value

          if (funcValue <= Satoshis.zero) {
            currentRange match {
              case StartZero(indexFrom, _) =>
                currentRange = StartZero(indexFrom, componentEnd)
              case _: StartTotal | _: StartFunc | _: StartFuncConst =>
                rangeBuilder += currentRange
                currentRange = StartZero(componentStart, componentEnd)
            }
          } else if (funcValue >= totalCollateral) {
            currentRange match {
              case StartTotal(indexFrom, _) =>
                currentRange = StartTotal(indexFrom, componentEnd)
              case _: StartZero | _: StartFunc | _: StartFuncConst =>
                rangeBuilder += currentRange
                currentRange = StartTotal(componentStart, componentEnd)
            }
          } else if (num != from && funcValue == prevFunc(num - 1, rounding)) {
            currentRange match {
              case StartFunc(indexFrom, indexTo) =>
                rangeBuilder += StartFunc(indexFrom, indexTo - 1)
                currentRange = StartFuncConst(indexTo, componentEnd)
              case StartFuncConst(indexFrom, _) =>
                currentRange = StartFuncConst(indexFrom, componentEnd)
              case _: StartZero | _: StartTotal =>
                throw new RuntimeException("Something has gone horribly wrong.")
            }
          } else {
            rangeBuilder += currentRange
            currentRange = StartFuncConst(componentStart, componentEnd)
          }

          num = componentEnd + 1
          if (num != to) {
            updateComponent()
            processConstantComponents()
          }
        case _: OutcomeValueFunctionComponent => ()
      }
    }

    processConstantComponents()

    while (num <= to) {
      if (num == currentFunc.rightEndpoint.outcome && num != to) {
        updateComponent()

        processConstantComponents()
      }

      val value = currentFunc(num, rounding)
      if (value <= Satoshis.zero) {
        currentRange match {
          case StartZero(indexFrom, _) =>
            currentRange = StartZero(indexFrom, num)
          case _: StartTotal | _: StartFunc | _: StartFuncConst =>
            newRange(value)
        }
      } else if (value >= totalCollateral) {
        currentRange match {
          case StartTotal(indexFrom, _) =>
            currentRange = StartTotal(indexFrom, num)
          case _: StartZero | _: StartFunc | _: StartFuncConst =>
            newRange(value)
        }
      } else if (
        num != from &&
        (num - 1 >= componentStart && value == currentFunc(num - 1,
                                                           rounding)) ||
        (num - 1 < componentStart && value == prevFunc(num - 1, rounding))
      ) {
        currentRange match {
          case StartFunc(indexFrom, indexTo) =>
            rangeBuilder += StartFunc(indexFrom, indexTo - 1)
            currentRange = StartFuncConst(num - 1, num)
          case StartFuncConst(indexFrom, _) =>
            currentRange = StartFuncConst(indexFrom, num)
          case _: StartZero | _: StartTotal =>
            throw new RuntimeException("Something has gone horribly wrong.")
        }
      } else {
        currentRange match {
          case StartFunc(indexFrom, _) =>
            currentRange = StartFunc(indexFrom, num)
          case _: StartZero | _: StartTotal | _: StartFuncConst =>
            newRange(value)
        }
      }

      num += 1
    }

    rangeBuilder += currentRange

    rangeBuilder.result()
  }

  /** Searches for an outcome which contains a prefix of digits */
  def searchForPrefix[Outcome](digits: Vector[Int], outcomes: Vector[Outcome])(
      outcomeToPrefix: Outcome => Vector[Int]): Option[Outcome] = {
    val indexOrOverByOne = NumberUtil.search(outcomes, digits, outcomeToPrefix)(
      NumberUtil.lexicographicalOrdering[Int])

    if (indexOrOverByOne == outcomes.length) {
      if (digits.startsWith(outcomeToPrefix(outcomes.last))) {
        Some(outcomes.last)
      } else None
    } else if (indexOrOverByOne == 0) {
      if (digits.startsWith(outcomeToPrefix(outcomes.head))) {
        Some(outcomes.head)
      } else None
    } else if (digits == outcomeToPrefix(outcomes(indexOrOverByOne))) {
      Some(outcomes(indexOrOverByOne))
    } else {
      if (digits.startsWith(outcomeToPrefix(outcomes(indexOrOverByOne - 1)))) {
        Some(outcomes(indexOrOverByOne - 1))
      } else None
    }
  }

  /** Searches for an UnsignedNumericOutcome corresponding to (prefixing) digits */
  def searchForNumericOutcome(
      digits: Vector[Int],
      outcomes: Vector[DLCOutcomeType]): Option[UnsignedNumericOutcome] = {
    searchForPrefix(digits, outcomes) {
      case outcome: EnumOutcome =>
        throw new IllegalArgumentException(
          s"Expected Numeric Outcome, got $outcome")
      case UnsignedNumericOutcome(digits) => digits
    }.asInstanceOf[Option[UnsignedNumericOutcome]]
  }

  /** Computes the front groupings in the CETCompression
    * (with endpoint optimization but without total optimization).
    * This means the resulting outcomes cover [start, (prefix, digits[0], base-1, ..., base-1)].
    *
    * @param digits The unique digits of the range's start
    * @param base The base the digits are represented in
    */
  def frontGroupings(digits: Vector[Int], base: Int): Vector[Vector[Int]] = {
    val nonZeroDigits =
      digits.reverse.zipWithIndex.dropWhile(_._1 == 0) // Endpoint Optimization

    if (nonZeroDigits.isEmpty) { // All digits are 0
      Vector(Vector(0))
    } else {
      val fromFront = nonZeroDigits.init.flatMap {
        case (lastImportantDigit, unimportantDigits) =>
          val fixedDigits = digits.dropRight(unimportantDigits + 1)
          (lastImportantDigit + 1).until(base).map { lastDigit =>
            fixedDigits :+ lastDigit
          }
      }

      nonZeroDigits.map(_._1).reverse +: fromFront // Add Endpoint
    }
  }

  /** Computes the back groupings in the CETCompression
    * (with endpoint optimization but without total optimization).
    * This means the resulting outcomes cover [(prefix, digits[0], 0, ..., 0), end].
    *
    * @param digits The unique digits of the range's end
    * @param base The base the digits are represented in
    */
  def backGroupings(digits: Vector[Int], base: Int): Vector[Vector[Int]] = {
    val nonMaxDigits =
      digits.reverse.zipWithIndex.dropWhile(
        _._1 == base - 1
      ) // Endpoint Optimization

    if (nonMaxDigits.isEmpty) { // All digits are max
      Vector(Vector(base - 1))
    } else {
      // Here we compute the back groupings in reverse so as to use the same iteration as in front groupings
      val fromBack = nonMaxDigits.init.flatMap {
        case (lastImportantDigit, unimportantDigits) =>
          val fixedDigits = digits.dropRight(unimportantDigits + 1)
          0.until(lastImportantDigit)
            .reverse
            .toVector
            .map { lastDigit =>
              fixedDigits :+ lastDigit
            }
      }

      fromBack.reverse :+ nonMaxDigits.map(_._1).reverse // Add Endpoint
    }
  }

  /** Computes the middle groupings in the CETCompression (without total optimization).
    * This means the resulting outcomes cover
    * [(prefix, firstDigitStart + 1, 0, ..., 0), (prefix, firstDigitEnd-1, base-1, ..., base-1)].
    *
    * @param firstDigitStart The first unique digit of the range's start
    * @param firstDigitEnd The first unique digit of the range's end
    */
  def middleGrouping(
      firstDigitStart: Int,
      firstDigitEnd: Int): Vector[Vector[Int]] = {
    (firstDigitStart + 1).until(firstDigitEnd).toVector.map { firstDigit =>
      Vector(firstDigit)
    }
  }

  /** Splits off the shared prefix of start and end represented in the given base
    * and returns the shared prefix and the unique digits of start and of end.
    */
  def separatePrefix(
      start: Long,
      end: Long,
      base: Int,
      numDigits: Int): (Vector[Int], Vector[Int], Vector[Int]) = {
    val startDigits = NumberUtil.decompose(start, base, numDigits)
    val endDigits = NumberUtil.decompose(end, base, numDigits)

    val prefixDigits = startDigits
      .zip(endDigits)
      .takeWhile { case (startDigit, endDigit) => startDigit == endDigit }
      .map(_._1)

    (prefixDigits,
     startDigits.drop(prefixDigits.length),
     endDigits.drop(prefixDigits.length))
  }

  /** Runs the compression algorithm with all optimizations on the interval [start, end]
    * represented in the given base.
    */
  def groupByIgnoringDigits(
      start: Long,
      end: Long,
      base: Int,
      numDigits: Int): Vector[Vector[Int]] = {
    val (prefixDigits, startDigits, endDigits) =
      separatePrefix(start, end, base, numDigits)

    if (start == end) { // Special Case: Range Length 1
      Vector(prefixDigits)
    } else if (startDigits.forall(_ == 0) && endDigits.forall(_ == base - 1)) { // Total Optimization
      Vector(prefixDigits)
    } else if (prefixDigits.length == numDigits - 1) { // Special Case: Front Grouping = Back Grouping
      startDigits.last.to(endDigits.last).toVector.map { lastDigit =>
        prefixDigits :+ lastDigit
      }
    } else {
      val front = frontGroupings(startDigits, base)
      val middle = middleGrouping(startDigits.head, endDigits.head)
      val back = backGroupings(endDigits, base)

      val groupings = front ++ middle ++ back

      groupings.map { digits =>
        prefixDigits ++ digits
      }
    }
  }

  /** Computes the compressed set of outcomes and their corresponding payouts given
    * a base, the number of digits to be signed, the payout function, totalCollateral
    * and the range of outcomes to construct CETs for, [min, max].
    */
  def computeCETs(
      base: Int,
      numDigits: Int,
      function: OutcomeValueFunction,
      totalCollateral: Satoshis,
      rounding: RoundingIntervals,
      min: Long,
      max: Long): Vector[(Vector[Int], Satoshis)] = {
    val ranges = splitIntoRanges(min, max, totalCollateral, function, rounding)

    ranges.flatMap { range =>
      range match {
        case StartZero(indexFrom, indexTo) =>
          groupByIgnoringDigits(indexFrom, indexTo, base, numDigits).map(
            _ -> Satoshis.zero)
        case StartTotal(indexFrom, indexTo) =>
          groupByIgnoringDigits(indexFrom, indexTo, base, numDigits).map(
            _ -> totalCollateral)
        case StartFuncConst(indexFrom, indexTo) =>
          groupByIgnoringDigits(indexFrom, indexTo, base, numDigits).map(
            _ -> function(indexFrom, rounding))
        case StartFunc(indexFrom, indexTo) =>
          indexFrom.to(indexTo).map { num =>
            NumberUtil.decompose(num, base, numDigits) -> function(num)
          }
      }
    }
  }

  /** Computes the compressed set of outcomes and their corresponding payouts given
    * a base, the number of digits to be signed, the payout function, and totalCollateral.
    */
  def computeCETs(
      base: Int,
      numDigits: Int,
      function: OutcomeValueFunction,
      totalCollateral: Satoshis,
      rounding: RoundingIntervals): Vector[(Vector[Int], Satoshis)] = {
    val min = 0
    val max = Math.pow(base, numDigits).toLong - 1

    computeCETs(base, numDigits, function, totalCollateral, rounding, min, max)
  }
}
