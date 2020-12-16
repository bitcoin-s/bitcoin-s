package org.bitcoins.core.protocol.dlc

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.util.NumberUtil

import scala.math.BigDecimal.RoundingMode

/** A DLC payout curve defined by piecewise interpolating points */
case class DLCPayoutCurve(points: Vector[OutcomePayoutPoint]) {
  require(points.init.zip(points.tail).forall {
            case (p1, p2) => p1.outcome < p2.outcome
          },
          s"Points must be ascending: $points")

  /** These points (and their indices in this.points) represent the endpoints
    * between which interpolation happens.
    * In other words these endpoints define the pieces of the piecewise function.
    */
  lazy val endpoints: Vector[(OutcomePayoutPoint, Int)] =
    points.zipWithIndex.filter(_._1.isEndpoint)

  /** This Vector contains the function pieces between the endpoints */
  lazy val functionComponents: Vector[DLCPayoutCurveComponent] = {
    endpoints.init.zip(endpoints.tail).map { // All pairs of adjacent endpoints
      case ((_, index), (_, nextIndex)) =>
        DLCPayoutCurveComponent(points.slice(index, nextIndex + 1))
    }
  }

  private lazy val outcomes = endpoints.map(_._1.outcome)

  /** Returns the function component on which the given oracle outcome is
    * defined, along with its index
    */
  def componentFor(outcome: BigDecimal): (DLCPayoutCurveComponent, Int) = {
    val endpointIndex = NumberUtil.search(outcomes, outcome)
    val (endpoint, _) = endpoints(endpointIndex)

    if (
      endpoint.outcome == outcome && endpointIndex != functionComponents.length
    ) {
      (functionComponents(endpointIndex), endpointIndex)
    } else {
      (functionComponents(endpointIndex - 1), endpointIndex - 1)
    }
  }

  def getPayout(outcome: BigDecimal): Satoshis = {
    val (func, _) = componentFor(outcome)
    func(outcome)
  }

  def getPayout(outcome: BigDecimal, rounding: RoundingIntervals): Satoshis = {
    val (func, _) = componentFor(outcome)
    func(outcome, rounding)
  }

  def apply(outcome: BigDecimal): Satoshis = getPayout(outcome)

  def apply(outcome: BigDecimal, rounding: RoundingIntervals): Satoshis =
    getPayout(outcome, rounding)
}

/** A point on a DLC payout curve to be used for interpolation
  *
  * @param outcome An element of the domain of possible events signed by the oracle
  * @param payout The payout to the local party corresponding to outcome
  * @param isEndpoint True if this point defines a boundary between pieces in the curve
  */
case class OutcomePayoutPoint(
    outcome: BigDecimal,
    payout: Satoshis,
    isEndpoint: Boolean)

/** A single piece of a larger piecewise function defined between left and right endpoints */
sealed trait DLCPayoutCurveComponent {
  def leftEndpoint: OutcomePayoutPoint
  def midpoints: Vector[OutcomePayoutPoint]
  def rightEndpoint: OutcomePayoutPoint

  require(leftEndpoint.isEndpoint, s"$leftEndpoint not an endpoint")
  require(rightEndpoint.isEndpoint, s"$rightEndpoint not an endpoint")
  require(midpoints.forall(!_.isEndpoint), s"$midpoints contained an endpoint")
  midpoints.headOption match {
    case Some(firstMidpoint) =>
      require(leftEndpoint.outcome < firstMidpoint.outcome,
              s"Points must be ascending: $this")
      require(midpoints.init.zip(midpoints.tail).forall {
                case (m1, m2) => m1.outcome < m2.outcome
              },
              s"Points must be ascending: $this")
      require(rightEndpoint.outcome > midpoints.last.outcome,
              s"Points must be ascending: $this")
    case None =>
      require(leftEndpoint.outcome < rightEndpoint.outcome,
              s"Points must be ascending: $this")
  }

  def apply(outcome: BigDecimal): Satoshis

  def apply(outcome: BigDecimal, rounding: RoundingIntervals): Satoshis = {
    rounding.round(outcome, apply(outcome))
  }

  /** Returns the largest Long less than or equal to bd (floor function) */
  protected def bigDecimalSats(bd: BigDecimal): Satoshis = {
    Satoshis(bd.setScale(0, RoundingMode.FLOOR).toLongExact)
  }
}

object DLCPayoutCurveComponent {

  def apply(points: Vector[OutcomePayoutPoint]): DLCPayoutCurveComponent = {
    points match {
      case Vector(left, right) =>
        if (left.payout == right.payout) {
          OutcomePayoutConstant(left, right)
        } else {
          OutcomePayoutLine(left, right)
        }
      case Vector(left, mid, right) => OutcomePayoutQuadratic(left, mid, right)
      case Vector(left, mid1, mid2, right) =>
        OutcomePayoutCubic(left, mid1, mid2, right)
      case _ => OutcomePayoutPolynomial(points)
    }
  }
}

case class OutcomePayoutConstant(
    leftEndpoint: OutcomePayoutPoint,
    rightEndpoint: OutcomePayoutPoint)
    extends DLCPayoutCurveComponent {
  require(leftEndpoint.payout == rightEndpoint.payout,
          "Constant function must have same values on endpoints")

  override lazy val midpoints: Vector[OutcomePayoutPoint] = Vector.empty

  override def apply(outcome: BigDecimal): Satoshis = leftEndpoint.payout
}

/** A Line between left and right endpoints defining a piece of a larger payout curve */
case class OutcomePayoutLine(
    leftEndpoint: OutcomePayoutPoint,
    rightEndpoint: OutcomePayoutPoint)
    extends DLCPayoutCurveComponent {
  override lazy val midpoints: Vector[OutcomePayoutPoint] = Vector.empty

  lazy val slope: BigDecimal = {
    (rightEndpoint.payout.toLong - leftEndpoint.payout.toLong) / (rightEndpoint.outcome - leftEndpoint.outcome)
  }

  override def apply(outcome: BigDecimal): Satoshis = {
    val value =
      (outcome - leftEndpoint.outcome) * slope + leftEndpoint.payout.toLong

    bigDecimalSats(value)
  }
}

/** A quadratic between left and right endpoints defining a piece of a larger payout curve.
  * A quadratic equation defines a parabola: https://en.wikipedia.org/wiki/Quadratic_function
  */
case class OutcomePayoutQuadratic(
    leftEndpoint: OutcomePayoutPoint,
    midpoint: OutcomePayoutPoint,
    rightEndpoint: OutcomePayoutPoint)
    extends DLCPayoutCurveComponent {
  override lazy val midpoints: Vector[OutcomePayoutPoint] = Vector(midpoint)

  private lazy val (x01, x02, x12) =
    (leftEndpoint.outcome - midpoint.outcome,
     leftEndpoint.outcome - rightEndpoint.outcome,
     midpoint.outcome - rightEndpoint.outcome)

  private lazy val (x10, x20, x21) = (-x01, -x02, -x12)

  private lazy val (y0, y1, y2) = (leftEndpoint.payout.toLong,
                                   midpoint.payout.toLong,
                                   rightEndpoint.payout.toLong)

  private lazy val (c0, c1, c2) =
    (y0 / (x01 * x02), y1 / (x10 * x12), y2 / (x20 * x21))

  override def apply(outcome: BigDecimal): Satoshis = {
    val x0 = outcome - leftEndpoint.outcome
    val x1 = outcome - midpoint.outcome
    val x2 = outcome - rightEndpoint.outcome

    val value = c0 * (x1 * x2) + c1 * (x0 * x2) + c2 * (x0 * x1)

    bigDecimalSats(value)
  }
}

/** A cubic between left and right endpoints defining a piece of a larger payout curve */
case class OutcomePayoutCubic(
    leftEndpoint: OutcomePayoutPoint,
    leftMidpoint: OutcomePayoutPoint,
    rightMidpoint: OutcomePayoutPoint,
    rightEndpoint: OutcomePayoutPoint)
    extends DLCPayoutCurveComponent {

  override lazy val midpoints: Vector[OutcomePayoutPoint] =
    Vector(leftMidpoint, rightMidpoint)

  private lazy val (x01, x02, x03, x12, x13, x23) =
    (leftEndpoint.outcome - leftMidpoint.outcome,
     leftEndpoint.outcome - rightMidpoint.outcome,
     leftEndpoint.outcome - rightEndpoint.outcome,
     leftMidpoint.outcome - rightMidpoint.outcome,
     leftMidpoint.outcome - rightEndpoint.outcome,
     rightMidpoint.outcome - rightEndpoint.outcome)

  private lazy val (x10, x20, x30, x21, x31, x32) =
    (-x01, -x02, -x03, -x12, -x13, -x23)

  private lazy val (y0, y1, y2, y3) = (leftEndpoint.payout.toLong,
                                       leftMidpoint.payout.toLong,
                                       rightMidpoint.payout.toLong,
                                       rightEndpoint.payout.toLong)

  private lazy val (c0, c1, c2, c3) =
    (y0 / (x01 * x02 * x03),
     y1 / (x10 * x12 * x13),
     y2 / (x20 * x21 * x23),
     y3 / (x30 * x31 * x32))

  override def apply(outcome: BigDecimal): Satoshis = {
    val x0 = outcome - leftEndpoint.outcome
    val x1 = outcome - leftMidpoint.outcome
    val x2 = outcome - rightMidpoint.outcome
    val x3 = outcome - rightEndpoint.outcome

    val value =
      c0 * (x1 * x2 * x3) + c1 * (x0 * x2 * x3) + c2 * (x0 * x1 * x3) + c3 * (x0 * x1 * x2)

    bigDecimalSats(value)
  }
}

/** A polynomial interpolating points and defining a piece of a larger payout curve */
case class OutcomePayoutPolynomial(points: Vector[OutcomePayoutPoint])
    extends DLCPayoutCurveComponent {
  override lazy val leftEndpoint: OutcomePayoutPoint = points.head
  override lazy val rightEndpoint: OutcomePayoutPoint = points.last
  override lazy val midpoints: Vector[OutcomePayoutPoint] = points.tail.init

  lazy val coefficients: Vector[BigDecimal] = {
    points.map {
      case OutcomePayoutPoint(xi, yi, _) =>
        val denom = points.foldLeft(BigDecimal(1)) {
          case (prodSoFar, OutcomePayoutPoint(xj, _, _)) =>
            if (xj == xi) {
              prodSoFar
            } else {
              prodSoFar * (xi - xj)
            }
        }

        yi.toLong / denom
    }
  }

  override def apply(outcome: BigDecimal): Satoshis = {
    points.find(_.outcome == outcome) match {
      case Some(point) => point.payout
      case None =>
        val allProd = points.foldLeft(BigDecimal(1)) {
          case (prodSoFar, OutcomePayoutPoint(xj, _, _)) =>
            prodSoFar * (outcome - xj)
        }

        val value = coefficients.zipWithIndex.foldLeft(BigDecimal(0)) {
          case (sumSoFar, (coefficientI, i)) =>
            sumSoFar + (coefficientI * allProd / (outcome - points(i).outcome))
        }

        bigDecimalSats(value)
    }
  }
}
