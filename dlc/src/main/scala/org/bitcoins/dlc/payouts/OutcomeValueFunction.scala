package org.bitcoins.dlc.payouts

import org.bitcoins.core.currency.Satoshis

import scala.math.BigDecimal.RoundingMode

case class OutcomeValueFunction(points: Vector[OutcomeValuePoint]) {

  lazy val endpoints: Vector[(OutcomeValuePoint, Int)] =
    points.zipWithIndex.filter(_._1.isEndpoint)

  lazy val functionComponents: Vector[OutcomeValueFunctionComponent] = {
    endpoints.init.zip(endpoints.tail).map {
      case ((_, index), (_, nextIndex)) =>
        OutcomeValueFunctionComponent(points.slice(index, nextIndex + 1))
    }
  }

  private lazy val outcomes = points.map(_.outcome)

  def apply(outcome: BigDecimal): Satoshis = {
    val endpointIndex = outcomes.search(outcome).insertionPoint
    val (endpoint, _) = endpoints(endpointIndex)

    if (endpoint.outcome == outcome) {
      endpoint.value
    } else {
      val func = functionComponents(endpointIndex - 1)
      func(outcome)
    }
  }
}

case class OutcomeValuePoint(
    outcome: BigDecimal,
    value: Satoshis,
    isEndpoint: Boolean)

sealed trait OutcomeValueFunctionComponent {
  def leftEndpoint: OutcomeValuePoint
  def midpoints: Vector[OutcomeValuePoint]
  def rightEndpoint: OutcomeValuePoint

  require(leftEndpoint.isEndpoint, s"$leftEndpoint not an endpoint")
  require(rightEndpoint.isEndpoint, s"$rightEndpoint not an endpoint")
  require(midpoints.forall(!_.isEndpoint), s"$midpoints contained an endpoint")

  def apply(outcome: BigDecimal): Satoshis

  protected def bigDecimalSats(bd: BigDecimal): Satoshis = {
    Satoshis(bd.setScale(0, RoundingMode.FLOOR).toLongExact)
  }
}

object OutcomeValueFunctionComponent {

  def apply(
      points: Vector[OutcomeValuePoint]): OutcomeValueFunctionComponent = {
    points match {
      case Vector(left, right)      => OutcomeValueLine(left, right)
      case Vector(left, mid, right) => OutcomeValueQuadratic(left, mid, right)
      case _                        => OutcomeValuePolynomial(points)
    }
  }
}

case class OutcomeValueLine(
    leftEndpoint: OutcomeValuePoint,
    rightEndpoint: OutcomeValuePoint)
    extends OutcomeValueFunctionComponent {
  override def midpoints: Vector[OutcomeValuePoint] = Vector.empty

  lazy val slope: BigDecimal = {
    (rightEndpoint.value.toLong - leftEndpoint.value.toLong) / (rightEndpoint.outcome - leftEndpoint.outcome)
  }

  override def apply(outcome: BigDecimal): Satoshis = {
    val value =
      (outcome - leftEndpoint.outcome) * slope + leftEndpoint.value.toLong

    bigDecimalSats(value)
  }
}

case class OutcomeValueQuadratic(
    leftEndpoint: OutcomeValuePoint,
    midpoint: OutcomeValuePoint,
    rightEndpoint: OutcomeValuePoint)
    extends OutcomeValueFunctionComponent {
  override def midpoints: Vector[OutcomeValuePoint] = Vector(midpoint)

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

case class OutcomeValuePolynomial(points: Vector[OutcomeValuePoint])
    extends OutcomeValueFunctionComponent {
  override val leftEndpoint: OutcomeValuePoint = points.head
  override val rightEndpoint: OutcomeValuePoint = points.last
  override val midpoints: Vector[OutcomeValuePoint] = points.tail.init

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
