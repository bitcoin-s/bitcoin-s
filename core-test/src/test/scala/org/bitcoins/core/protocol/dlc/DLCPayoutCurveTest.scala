package org.bitcoins.core.protocol.dlc

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.testkitcore.util.BitcoinSUnitTest
import org.scalacheck.Gen

import scala.math.BigDecimal.RoundingMode

class DLCPayoutCurveTest extends BitcoinSUnitTest {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  behavior of "DLCPayoutCurve"

  private val numGen = Gen.choose[Long](0, 10000)
  private val intGen = Gen.choose[Int](0, 1000)

  def nPoints(n: Int): Gen[Vector[OutcomePayoutPoint]] = {
    val valueGen = Gen.choose[Long](0, 10000)
    val pointGen = for {
      outcome <- numGen
      value <- valueGen
    } yield OutcomePayoutEndpoint(outcome, Satoshis(value))
    Gen
      .listOfN(n, pointGen)
      .suchThat(points =>
        points.map(_.outcome).distinct.length == points.length)
      .map(_.toVector.sortBy(_.outcome))
      .map { points =>
        points.head +: points.tail.init.map(_.toMidpoint) :+ points.last
      }
  }

  it should "agree on lines and degree 1 polynomials" in {
    forAll(nPoints(2), Gen.listOfN(1000, numGen)) {
      case (Vector(point1: OutcomePayoutEndpoint,
                   point2: OutcomePayoutEndpoint),
            outcomes) =>
        val line = OutcomePayoutLine(point1, point2)
        val polyDegOne = OutcomePayoutPolynomial(Vector(point1, point2))

        outcomes.foreach { outcome =>
          assert(line(outcome) == polyDegOne(outcome))
        }

      case _ => fail()
    }
  }

  it should "agree on lines and y = mx + b" in {
    val twoNums = for {
      num1 <- intGen
      num2 <- intGen.suchThat(_ != num1)
    } yield {
      if (num1 < num2) {
        (num1, num2)
      } else {
        (num2, num1)
      }
    }

    forAll(intGen, intGen, Gen.listOfN(1000, numGen), twoNums) {
      case (slope, yIntercept, outcomes, (x1, x2)) =>
        def expectedPayout(outcome: BigDecimal): Satoshis = {
          val value = slope * outcome + yIntercept
          val rounded = value.setScale(0, RoundingMode.FLOOR).toLongExact
          Satoshis(rounded)
        }

        val point1 = OutcomePayoutEndpoint(x1, expectedPayout(x1))
        val point2 = OutcomePayoutEndpoint(x2, expectedPayout(x2))
        val line = OutcomePayoutLine(point1, point2)

        outcomes.foreach { outcome =>
          assert(line(outcome) == expectedPayout(outcome))
        }
    }
  }

  it should "agree on quadratics and degree 2 polynomials" in {
    forAll(nPoints(3), Gen.listOfN(1000, numGen)) {
      case (Vector(point1: OutcomePayoutEndpoint,
                   point2: OutcomePayoutMidpoint,
                   point3: OutcomePayoutEndpoint),
            outcomes) =>
        val parabola = OutcomePayoutQuadratic(point1, point2, point3)
        val polyDegTwo =
          OutcomePayoutPolynomial(Vector(point1, point2, point3))

        outcomes.foreach { outcome =>
          assert(parabola(outcome) == polyDegTwo(outcome))
        }

      case _ => fail()
    }
  }

  it should "agree on quadratics and y = ax^2 + bx + c" in {
    val threeNums = for {
      num1 <- intGen
      num2 <- intGen.suchThat(_ != num1)
      num3 <- intGen.suchThat(x => x != num1 && x != num2)
    } yield {
      val nums = Vector(num1, num2, num3).sorted
      (nums(0), nums(1), nums(2))
    }

    forAll(intGen, intGen, intGen, Gen.listOfN(1000, numGen), threeNums) {
      case (a, b, c, outcomes, (x1, x2, x3)) =>
        def expectedPayout(outcome: BigDecimal): Satoshis = {
          val value = a * outcome * outcome + b * outcome + c
          val rounded = value.setScale(0, RoundingMode.FLOOR).toLongExact
          Satoshis(rounded)
        }

        val point1 = OutcomePayoutEndpoint(x1, expectedPayout(x1))
        val point2 = OutcomePayoutMidpoint(x2, expectedPayout(x2))
        val point3 = OutcomePayoutEndpoint(x3, expectedPayout(x3))
        val parabola = OutcomePayoutQuadratic(point1, point2, point3)

        outcomes.foreach { outcome =>
          assert(parabola(outcome) == expectedPayout(outcome))
        }
    }
  }

  it should "agree on degenerate quadratics and lines" in {
    val threeNums = for {
      num1 <- intGen
      num2 <- intGen.suchThat(_ != num1)
      num3 <- intGen.suchThat(x => x != num1 && x != num2)
    } yield {
      val nums = Vector(num1, num2, num3).sorted
      (nums(0), nums(1), nums(2))
    }

    forAll(intGen, intGen, Gen.listOfN(1000, numGen), threeNums) {
      case (slope, yIntercept, outcomes, (x1, x2, x3)) =>
        def expectedPayout(outcome: BigDecimal): Satoshis = {
          val value = slope * outcome + yIntercept
          val rounded = value.setScale(0, RoundingMode.FLOOR).toLongExact
          Satoshis(rounded)
        }

        val point1 = OutcomePayoutEndpoint(x1, expectedPayout(x1))
        val point2 = OutcomePayoutMidpoint(x2, expectedPayout(x2))
        val point3 = OutcomePayoutEndpoint(x3, expectedPayout(x3))
        val line = OutcomePayoutLine(point1, point3)
        val parabola = OutcomePayoutQuadratic(point1, point2, point3)

        outcomes.foreach { outcome =>
          assert(line(outcome) == parabola(outcome))
        }
    }
  }

  it should "agree on cubics and degree 3 polynomials" in {
    forAll(nPoints(4), Gen.listOfN(1000, numGen)) {
      case (Vector(point1: OutcomePayoutEndpoint,
                   point2: OutcomePayoutMidpoint,
                   point3: OutcomePayoutMidpoint,
                   point4: OutcomePayoutEndpoint),
            outcomes) =>
        val cubic = OutcomePayoutCubic(point1, point2, point3, point4)
        val polyDegThree =
          OutcomePayoutPolynomial(Vector(point1, point2, point3, point4))

        outcomes.foreach { outcome =>
          assert(cubic(outcome) == polyDegThree(outcome))
        }

      case _ => fail()
    }
  }

  it should "agree on cubics and y = ax^3 + bx^2 + cx + d" in {
    val fourNums = for {
      num1 <- intGen
      num2 <- intGen.suchThat(_ != num1)
      num3 <- intGen.suchThat(x => x != num1 && x != num2)
      num4 <- intGen.suchThat(x => x != num1 && x != num2 && x != num3)
    } yield {
      val nums = Vector(num1, num2, num3, num4).sorted
      (nums(0), nums(1), nums(2), nums(3))
    }

    forAll(intGen,
           intGen,
           intGen,
           intGen,
           Gen.listOfN(1000, numGen),
           fourNums) { case (a, b, c, d, outcomes, (x1, x2, x3, x4)) =>
      def expectedPayout(outcome: BigDecimal): Satoshis = {
        val value =
          a * outcome * outcome * outcome + b * outcome * outcome + c * outcome + d
        val rounded = value.setScale(0, RoundingMode.FLOOR).toLongExact
        Satoshis(rounded)
      }

      val point1 = OutcomePayoutEndpoint(x1, expectedPayout(x1))
      val point2 = OutcomePayoutMidpoint(x2, expectedPayout(x2))
      val point3 = OutcomePayoutMidpoint(x3, expectedPayout(x3))
      val point4 = OutcomePayoutEndpoint(x4, expectedPayout(x4))
      val cubic = OutcomePayoutCubic(point1, point2, point3, point4)

      outcomes.foreach { outcome =>
        assert(cubic(outcome) == expectedPayout(outcome))
      }
    }
  }

  it should "agree on degenerate cubics and lines" in {
    val fourNums = for {
      num1 <- intGen
      num2 <- intGen.suchThat(_ != num1)
      num3 <- intGen.suchThat(x => x != num1 && x != num2)
      num4 <- intGen.suchThat(x => x != num1 && x != num2 && x != num3)
    } yield {
      val nums = Vector(num1, num2, num3, num4).sorted
      (nums(0), nums(1), nums(2), nums(3))
    }

    forAll(intGen, intGen, Gen.listOfN(1000, numGen), fourNums) {
      case (slope, yIntercept, outcomes, (x1, x2, x3, x4)) =>
        def expectedPayout(outcome: BigDecimal): Satoshis = {
          val value = slope * outcome + yIntercept
          val rounded = value.setScale(0, RoundingMode.FLOOR).toLongExact
          Satoshis(rounded)
        }

        val point1 = OutcomePayoutEndpoint(x1, expectedPayout(x1))
        val point2 = OutcomePayoutMidpoint(x2, expectedPayout(x2))
        val point3 = OutcomePayoutMidpoint(x3, expectedPayout(x3))
        val point4 = OutcomePayoutEndpoint(x4, expectedPayout(x4))
        val line = OutcomePayoutLine(point1, point4)
        val cubic = OutcomePayoutCubic(point1, point2, point3, point4)

        outcomes.foreach { outcome =>
          assert(line(outcome) == cubic(outcome))
        }
    }
  }

  it should "agree on degenerate cubics and quadratics" in {
    val fourNums = for {
      num1 <- intGen
      num2 <- intGen.suchThat(_ != num1)
      num3 <- intGen.suchThat(x => x != num1 && x != num2)
      num4 <- intGen.suchThat(x => x != num1 && x != num2 && x != num3)
    } yield {
      val nums = Vector(num1, num2, num3, num4).sorted
      (nums(0), nums(1), nums(2), nums(3))
    }

    forAll(intGen, intGen, intGen, Gen.listOfN(1000, numGen), fourNums) {
      case (a, b, c, outcomes, (x1, x2, x3, x4)) =>
        def expectedPayout(outcome: BigDecimal): Satoshis = {
          val value = a * outcome * outcome + b * outcome + c
          val rounded = value.setScale(0, RoundingMode.FLOOR).toLongExact
          Satoshis(rounded)
        }

        val point1 = OutcomePayoutEndpoint(x1, expectedPayout(x1))
        val point2 = OutcomePayoutMidpoint(x2, expectedPayout(x2))
        val point3 = OutcomePayoutMidpoint(x3, expectedPayout(x3))
        val point4 = OutcomePayoutEndpoint(x4, expectedPayout(x4))
        val quadratic = OutcomePayoutQuadratic(point1, point2, point4)
        val cubic = OutcomePayoutCubic(point1, point2, point3, point4)

        outcomes.foreach { outcome =>
          assert(quadratic(outcome) == cubic(outcome))
        }
    }
  }

  it should "parse points into component functions correctly and compute outputs" in {
    val point0 = OutcomePayoutEndpoint(0, Satoshis.zero)
    val point1 = OutcomePayoutEndpoint(10, Satoshis(100))

    val line = DLCPayoutCurve(Vector(point0, point1))
    val lineFunc = line.functionComponents
    assert(lineFunc == Vector(OutcomePayoutLine(point0, point1)))

    val point2 = OutcomePayoutMidpoint(20, Satoshis.zero)
    val point3 = OutcomePayoutEndpoint(30, Satoshis(300))

    val quad = DLCPayoutCurve(Vector(point1, point2, point3))
    val quadFunc = quad.functionComponents
    assert(quadFunc == Vector(OutcomePayoutQuadratic(point1, point2, point3)))

    val point4 = OutcomePayoutMidpoint(40, Satoshis(600))
    val point5 = OutcomePayoutMidpoint(50, Satoshis(500))
    val point6 = OutcomePayoutEndpoint(60, Satoshis(700))
    val cubicPoints = Vector(point3, point4, point5, point6)

    val cubic = DLCPayoutCurve(cubicPoints)
    val cubicFunc = cubic.functionComponents
    assert(
      cubicFunc == Vector(OutcomePayoutCubic(point3, point4, point5, point6)))

    val func = DLCPayoutCurve(
      Vector(point0, point1, point2, point3, point4, point5, point6))
    val allFuncs = func.functionComponents
    assert(allFuncs == lineFunc ++ quadFunc ++ cubicFunc)

    forAll(Gen.choose[Long](0, 60)) { outcome =>
      val value = func(outcome)

      if (0 <= outcome && outcome < 10) {
        assert(value == line(outcome))
      } else if (10 <= outcome && outcome < 30) {
        assert(value == quad(outcome))
      } else {
        assert(value == cubic(outcome))
      }
    }
  }
}
