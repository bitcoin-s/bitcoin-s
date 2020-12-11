package org.bitcoins.commons.jsonmodels.dlc

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.util.NumberUtil

import scala.math.BigDecimal.RoundingMode

/** A DLC payout curve defined by piecewise interpolating points */
case class OutcomeValueFunction(points: Vector[OutcomeValuePoint]) {
  require(points.init.zip(points.tail).forall {
            case (p1, p2) => p1.outcome < p2.outcome
          },
          s"Points must be ascending: $points")

  /** These points (and their indices in this.points) represent the endpoints
    * between which interpolation happens.
    * In other words these endpoints define the pieces of the piecewise function.
    */
  lazy val endpoints: Vector[(OutcomeValuePoint, Int)] =
    points.zipWithIndex.filter(_._1.isEndpoint)

  /** This Vector contains the function pieces between the endpoints */
  lazy val functionComponents: Vector[OutcomeValueFunctionComponent] = {
    endpoints.init.zip(endpoints.tail).map { // All pairs of adjacent endpoints
      case ((_, index), (_, nextIndex)) =>
        OutcomeValueFunctionComponent(points.slice(index, nextIndex + 1))
    }
  }

  private lazy val outcomes = endpoints.map(_._1.outcome)

  /** Returns the function component on which the given oracle outcome is
    * defined, along with its index
    */
  def componentFor(
      outcome: BigDecimal): (OutcomeValueFunctionComponent, Int) = {
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
  * @param value The payout to the local party corresponding to outcome
  * @param isEndpoint True if this point defines a boundary between pieces in the curve
  */
case class OutcomeValuePoint(
    outcome: BigDecimal,
    value: Satoshis,
    isEndpoint: Boolean)

/** A single piece of a larger piecewise function defined between left and right endpoints */
sealed trait OutcomeValueFunctionComponent {
  def leftEndpoint: OutcomeValuePoint
  def midpoints: Vector[OutcomeValuePoint]
  def rightEndpoint: OutcomeValuePoint

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

object OutcomeValueFunctionComponent {

  def apply(
      points: Vector[OutcomeValuePoint]): OutcomeValueFunctionComponent = {
    points match {
      case Vector(left, right) =>
        if (left.value == right.value) {
          OutcomeValueConstant(left, right)
        } else {
          OutcomeValueLine(left, right)
        }
      case Vector(left, mid, right) => OutcomeValueQuadratic(left, mid, right)
      case Vector(left, mid1, mid2, right) =>
        OutcomeValueCubic(left, mid1, mid2, right)
      case _ => OutcomeValuePolynomial(points)
    }
  }
}

case class OutcomeValueConstant(
    leftEndpoint: OutcomeValuePoint,
    rightEndpoint: OutcomeValuePoint)
    extends OutcomeValueFunctionComponent {
  require(leftEndpoint.value == rightEndpoint.value,
          "Constant function must have same values on endpoints")

  override lazy val midpoints: Vector[OutcomeValuePoint] = Vector.empty

  override def apply(outcome: BigDecimal): Satoshis = leftEndpoint.value
}

/** A Line between left and right endpoints defining a piece of a larger payout curve */
case class OutcomeValueLine(
    leftEndpoint: OutcomeValuePoint,
    rightEndpoint: OutcomeValuePoint)
    extends OutcomeValueFunctionComponent {
  override lazy val midpoints: Vector[OutcomeValuePoint] = Vector.empty

  lazy val slope: BigDecimal = {
    (rightEndpoint.value.toLong - leftEndpoint.value.toLong) / (rightEndpoint.outcome - leftEndpoint.outcome)
  }

  override def apply(outcome: BigDecimal): Satoshis = {
    val value =
      (outcome - leftEndpoint.outcome) * slope + leftEndpoint.value.toLong

    bigDecimalSats(value)
  }
}

/** A quadratic between left and right endpoints defining a piece of a larger payout curve.
  * A quadratic equation defines a parabola: https://en.wikipedia.org/wiki/Quadratic_function
  */
case class OutcomeValueQuadratic(
    leftEndpoint: OutcomeValuePoint,
    midpoint: OutcomeValuePoint,
    rightEndpoint: OutcomeValuePoint)
    extends OutcomeValueFunctionComponent {
  override lazy val midpoints: Vector[OutcomeValuePoint] = Vector(midpoint)

  private lazy val (x01, x02, x12) =
    (leftEndpoint.outcome - midpoint.outcome,
     leftEndpoint.outcome - rightEndpoint.outcome,
     midpoint.outcome - rightEndpoint.outcome)

  private lazy val (x10, x20, x21) = (-x01, -x02, -x12)

  private lazy val (y0, y1, y2) = (leftEndpoint.value.toLong,
                                   midpoint.value.toLong,
                                   rightEndpoint.value.toLong)

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
case class OutcomeValueCubic(
    leftEndpoint: OutcomeValuePoint,
    leftMidpoint: OutcomeValuePoint,
    rightMidpoint: OutcomeValuePoint,
    rightEndpoint: OutcomeValuePoint)
    extends OutcomeValueFunctionComponent {

  override lazy val midpoints: Vector[OutcomeValuePoint] =
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

  private lazy val (y0, y1, y2, y3) = (leftEndpoint.value.toLong,
                                       leftMidpoint.value.toLong,
                                       rightMidpoint.value.toLong,
                                       rightEndpoint.value.toLong)

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
case class OutcomeValuePolynomial(points: Vector[OutcomeValuePoint])
    extends OutcomeValueFunctionComponent {
  override lazy val leftEndpoint: OutcomeValuePoint = points.head
  override lazy val rightEndpoint: OutcomeValuePoint = points.last
  override lazy val midpoints: Vector[OutcomeValuePoint] = points.tail.init

  lazy val coefficients: Vector[BigDecimal] = {
    points.map {
      case OutcomeValuePoint(xi, yi, _) =>
        val denom = points.foldLeft(BigDecimal(1)) {
          case (prodSoFar, OutcomeValuePoint(xj, _, _)) =>
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
      case Some(point) => point.value
      case None =>
        val allProd = points.foldLeft(BigDecimal(1)) {
          case (prodSoFar, OutcomeValuePoint(xj, _, _)) =>
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
