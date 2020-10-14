package org.bitcoins.dlc.payouts

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.dlc.payouts.CETCalculator._
import org.bitcoins.testkit.util.BitcoinSUnitTest
import org.scalacheck.Gen

import scala.annotation.tailrec

class CETCalculatorTest extends BitcoinSUnitTest {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  behavior of "CETCalculator"

  private val baseGen: Gen[Int] = Gen.choose(2, 256)

  it should "correctly split into ranges" in {
    val func = OutcomeValueFunction(
      Vector(
        OutcomeValuePoint(0, Satoshis(-1000), isEndpoint = true),
        OutcomeValuePoint(10, Satoshis(-1000), isEndpoint = true),
        OutcomeValuePoint(20, Satoshis(0), isEndpoint = false),
        OutcomeValuePoint(30, Satoshis(3000), isEndpoint = true),
        OutcomeValuePoint(40, Satoshis(4000), isEndpoint = true),
        OutcomeValuePoint(50, Satoshis(4000), isEndpoint = true),
        OutcomeValuePoint(70, Satoshis(0), isEndpoint = false),
        OutcomeValuePoint(80, Satoshis(1000), isEndpoint = true),
        OutcomeValuePoint(90, Satoshis(1000), isEndpoint = true),
        OutcomeValuePoint(100, Satoshis(11000), isEndpoint = false),
        OutcomeValuePoint(110, Satoshis(9000), isEndpoint = true)
      ))

    val expected = Vector(
      StartZero(0, 20),
      StartFunc(21, 39),
      StartFuncConst(40, 50),
      StartFunc(51, 69),
      StartZero(70, 70),
      StartFunc(71, 79),
      StartFuncConst(80, 90),
      StartFunc(91, 98),
      StartTotal(99, 108),
      StartFunc(109, 110)
    )

    val ranges = CETCalculator.splitIntoRanges(0, 110, Satoshis(10000), func)
    assert(ranges == expected)
  }

  it should "correctly decompose in any base" in {
    assert(CETCalculator.decompose(255, 16, 2) == Vector(15, 15))

    forAll(baseGen, Gen.choose(0L, Long.MaxValue)) {
      case (base, num) =>
        val numStr = num.toString
        val expectedBase10 = numStr
          .foldLeft(Vector.empty[Int]) {
            case (vec, char) =>
              vec :+ (char.toInt - '0'.toInt)
          }
          .reverse
        val base10 = CETCalculator.decompose(num, 10, numStr.length)
        assert(base10 == expectedBase10)

        // Add some extra digits for leading zeroes
        val numDigits = (Math.log(num) / Math.log(base)).toInt + 5
        val decomposed = CETCalculator.decompose(num, base, numDigits)
        assert(decomposed.last == 0)

        @tailrec
        def pow(base: BigInt, exp: Int, prodSoFar: BigInt = 1): BigInt = {
          if (exp == 0) {
            prodSoFar
          } else {
            pow(base, exp - 1, base * prodSoFar)
          }
        }

        val computedNum = decomposed.zipWithIndex.foldLeft(BigInt(0)) {
          case (sumSoFar, (digit, position)) =>
            sumSoFar + digit * pow(BigInt(base), position)
        }
        assert(computedNum.toLong == num)
    }
  }

  it should "correctly compute front groupings" in {
    val expected = Vector(
      Vector(1, 0, 9, 5),
      Vector(1, 0, 9, 6),
      Vector(1, 0, 9, 7),
      Vector(1, 0, 9, 8),
      Vector(1, 0, 9, 9),
      Vector(1, 1),
      Vector(1, 2),
      Vector(1, 3),
      Vector(1, 4),
      Vector(1, 5),
      Vector(1, 6),
      Vector(1, 7),
      Vector(1, 8),
      Vector(1, 9)
    )
    val frontGroupings =
      CETCalculator.frontGroupingsWithFixedDigit(Vector(5, 9, 0, 1), 10)
    assert(frontGroupings == expected)

    forAll(baseGen) { base =>
      val edgeCase =
        CETCalculator.frontGroupingsWithFixedDigit(Vector(0, 0, 0, 1), base)
      assert(edgeCase == Vector(Vector(1)))
    }
  }

  it should "correctly compute back groupings" in {
    val expected = Vector(
      Vector(1, 0, 0),
      Vector(1, 0, 1),
      Vector(1, 0, 2),
      Vector(1, 0, 3),
      Vector(1, 0, 4),
      Vector(1, 0, 5),
      Vector(1, 0, 6),
      Vector(1, 0, 7),
      Vector(1, 0, 8),
      Vector(1, 0, 9, 0),
      Vector(1, 0, 9, 1),
      Vector(1, 0, 9, 2),
      Vector(1, 0, 9, 3),
      Vector(1, 0, 9, 4),
      Vector(1, 0, 9, 5)
    )
    val backGroupings =
      CETCalculator.backGroupingsWithFixedDigit(Vector(5, 9, 0, 1), 10)
    assert(backGroupings == expected)

    forAll(baseGen) { base =>
      val edgeCase =
        CETCalculator.backGroupingsWithFixedDigit(
          Vector(base - 1, base - 1, base - 1, 1),
          base)
      assert(edgeCase == Vector(Vector(1)))
    }
  }

  it should "correctly compute first digit groupings" in {
    forAll(baseGen, baseGen) {
      case (digit1, digit2) =>
        val singleDigitGroupings =
          CETCalculator.firstDigitFixedGroupings(digit1, digit2)
        if (digit1 >= digit2 + 1) {
          assert(singleDigitGroupings == Vector.empty)
        } else {
          assert(
            singleDigitGroupings == (digit1 + 1).until(digit2).map(Vector(_)))
        }
    }
  }

  it should "correctly compute all groupings" in {
    val edgeCase = CETCalculator.groupByIgnoringDigits(startIndex = 123,
                                                       endIndex = 123,
                                                       base = 10,
                                                       numDigits = 3)
    assert(edgeCase == Vector(Vector(1, 2, 3)))

    val prefix = Vector(0, 1, 2, 0)

    val smallExpected = Vector(
      Vector(10, 11),
      Vector(10, 12),
      Vector(10, 13),
      Vector(10, 14),
      Vector(10, 15),
      Vector(11),
      Vector(12),
      Vector(13, 0),
      Vector(13, 1),
      Vector(13, 2)
    )

    val smallGroupings = CETCalculator.groupByIgnoringDigits(startIndex = 171,
                                                             endIndex = 210,
                                                             base = 16,
                                                             numDigits = 2)
    assert(smallGroupings == smallExpected)

    val smallExpectedWithPrefix = smallExpected.map(prefix ++ _)
    val smallGroupingsWithPrefix =
      CETCalculator.groupByIgnoringDigits(startIndex = 73899,
                                          endIndex = 73938,
                                          base = 16,
                                          numDigits = 6)
    assert(smallGroupingsWithPrefix == smallExpectedWithPrefix)

    val expected = Vector(
      Vector(1, 2, 3, 4),
      Vector(1, 2, 3, 5),
      Vector(1, 2, 3, 6),
      Vector(1, 2, 3, 7),
      Vector(1, 2, 3, 8),
      Vector(1, 2, 3, 9),
      Vector(1, 2, 4),
      Vector(1, 2, 5),
      Vector(1, 2, 6),
      Vector(1, 2, 7),
      Vector(1, 2, 8),
      Vector(1, 2, 9),
      Vector(1, 3),
      Vector(1, 4),
      Vector(1, 5),
      Vector(1, 6),
      Vector(1, 7),
      Vector(1, 8),
      Vector(1, 9),
      Vector(2),
      Vector(3),
      Vector(4, 0),
      Vector(4, 1),
      Vector(4, 2),
      Vector(4, 3, 0),
      Vector(4, 3, 1),
      Vector(4, 3, 2, 0),
      Vector(4, 3, 2, 1)
    )

    val groupings = CETCalculator.groupByIgnoringDigits(startIndex = 1234,
                                                        endIndex = 4321,
                                                        base = 10,
                                                        numDigits = 4)
    assert(groupings == expected)

    val expectedWithPrefix = expected.map(prefix ++ _)
    val groupingsWithPrefix =
      CETCalculator.groupByIgnoringDigits(startIndex = 1201234,
                                          endIndex = 1204321,
                                          base = 10,
                                          numDigits = 8)
    assert(groupingsWithPrefix == expectedWithPrefix)
  }

  it should "correctly compute all needed CETs" in {
    val func = OutcomeValueFunction(
      Vector(
        OutcomeValuePoint(0, Satoshis(-1000), isEndpoint = true),
        OutcomeValuePoint(10, Satoshis(-1000), isEndpoint = true),
        OutcomeValuePoint(20, Satoshis(0), isEndpoint = false),
        OutcomeValuePoint(30, Satoshis(3000), isEndpoint = true),
        OutcomeValuePoint(40, Satoshis(4000), isEndpoint = true),
        OutcomeValuePoint(50, Satoshis(4000), isEndpoint = true),
        OutcomeValuePoint(70, Satoshis(0), isEndpoint = false),
        OutcomeValuePoint(80, Satoshis(1000), isEndpoint = true),
        OutcomeValuePoint(90, Satoshis(1000), isEndpoint = true),
        OutcomeValuePoint(100, Satoshis(11000), isEndpoint = false),
        OutcomeValuePoint(110, Satoshis(9000), isEndpoint = true)
      ))

    val firstZeroRange = Vector(
      Vector(0, 0) -> Satoshis(0),
      Vector(0, 1) -> Satoshis(0),
      Vector(0, 2, 0) -> Satoshis(0)
    )

    val firstFuncRange = 21.until(40).toVector.map { num =>
      Vector(0, num / 10, num % 10) -> func(num)
    }

    val firstConstRange = Vector(
      Vector(0, 4) -> Satoshis(4000),
      Vector(0, 5, 0) -> Satoshis(4000)
    )

    val secondFuncRange = 51.until(80).toVector.map { num =>
      Vector(0, num / 10, num % 10) -> func(num)
    }

    val secondConstRange = Vector(
      Vector(0, 8) -> Satoshis(1000),
      Vector(0, 9, 0) -> Satoshis(1000)
    )

    val thirdFuncRange = 91.until(99).toVector.map { num =>
      Vector(0, num / 10, num % 10) -> func(num)
    }

    val firstTotalRange = Vector(
      Vector(0, 9, 9) -> Satoshis(10000),
      Vector(1, 0, 0) -> Satoshis(10000),
      Vector(1, 0, 1) -> Satoshis(10000),
      Vector(1, 0, 2) -> Satoshis(10000),
      Vector(1, 0, 3) -> Satoshis(10000),
      Vector(1, 0, 4) -> Satoshis(10000),
      Vector(1, 0, 5) -> Satoshis(10000),
      Vector(1, 0, 6) -> Satoshis(10000),
      Vector(1, 0, 7) -> Satoshis(10000),
      Vector(1, 0, 8) -> Satoshis(10000)
    )

    val fourthFuncRange = Vector(
      Vector(1, 0, 9) -> func(109),
      Vector(1, 1, 0) -> func(110)
    )

    val expected =
      firstZeroRange ++
        firstFuncRange ++
        firstConstRange ++
        secondFuncRange ++
        secondConstRange ++
        thirdFuncRange ++
        firstTotalRange ++
        fourthFuncRange

    val cetOutcomes = CETCalculator.computeCETs(base = 10,
                                                numDigits = 3,
                                                function = func,
                                                totalCollateral =
                                                  Satoshis(10000),
                                                min = 0,
                                                max = 110)
    assert(cetOutcomes == expected)
  }
}
