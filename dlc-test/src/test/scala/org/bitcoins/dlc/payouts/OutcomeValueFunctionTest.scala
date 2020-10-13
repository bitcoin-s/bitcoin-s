package org.bitcoins.dlc.payouts

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.testkit.util.BitcoinSUnitTest
import org.scalacheck.Gen

class OutcomeValueFunctionTest extends BitcoinSUnitTest {
  behavior of "OutcomeValueFunction"

  it should "agree on lines and degree 1 polynomials" in {
    val outcomeGen = Gen.choose[Double](0, 1000).map(BigDecimal(_))
    val valueGen = Gen.choose[Long](0, 10000)
    val pointGen = for {
      outcome <- outcomeGen
      value <- valueGen
    } yield OutcomeValuePoint(outcome, Satoshis(value), isEndpoint = true)
    val pointPairGen = for {
      pointA <- pointGen
      pointB <- pointGen.suchThat(point => point.outcome != pointA.outcome)
    } yield {
      if (pointA.outcome < pointB.outcome) {
        (pointA, pointB)
      } else {
        (pointB, pointA)
      }
    }

    forAll(pointPairGen, Gen.listOfN(1000, outcomeGen)) {
      case ((point1, point2), outcomes) =>
        val line = OutcomeValueLine(point1, point2)
        val polyDegOne = OutcomeValuePolynomial(Vector(point1, point2))

        outcomes.foreach { outcome =>
          assert(line(outcome) == polyDegOne(outcome))
        }
    }
  }
}
