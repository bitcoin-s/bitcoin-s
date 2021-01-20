package org.bitcoins.core.protocol.dlc

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.tlv.TLVPoint
import org.bitcoins.core.util.{Indexed, NumberUtil}

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
    *
    * It's important to note that the index returned here is relative to the _entire_
    * set of points, not the index relative to the set of endpoints.
    */
  lazy val endpoints: Vector[Indexed[OutcomePayoutEndpoint]] = {
    val endpoints = points.zipWithIndex.collect {
      case (o: OutcomePayoutEndpoint, idx) => (o, idx)
    }
    Indexed.fromGivenIndex(endpoints)
  }

  /** This Vector contains the function pieces between the endpoints */
  lazy val functionComponents: Vector[DLCPayoutCurveComponent] = {
    val zipped: Vector[
      (Indexed[OutcomePayoutEndpoint], Indexed[OutcomePayoutEndpoint])] =
      endpoints.init.zip(endpoints.tail)
    zipped.map { // All pairs of adjacent endpoints
      case (Indexed(_, index), Indexed(_, nextIndex)) =>
        val slice = points.slice(index, nextIndex + 1)
        DLCPayoutCurveComponent(slice)
    }
  }

  private lazy val outcomes = endpoints.map(_.element.outcome)

  /** Returns the function component on which the given oracle outcome is
    * defined, along with its index
    */
  def componentFor(outcome: Long): Indexed[DLCPayoutCurveComponent] = {
    val endpointIndex = NumberUtil.search(outcomes, outcome)
    val Indexed(endpoint, _) = endpoints(endpointIndex)

    if (
      endpoint.outcome == outcome && endpointIndex != functionComponents.length
    ) {
      Indexed(functionComponents(endpointIndex), endpointIndex)
    } else {
      Indexed(functionComponents(endpointIndex - 1), endpointIndex - 1)
    }
  }

  def getPayout(outcome: Long): Satoshis = {
    val Indexed(func, _) = componentFor(outcome)
    func(outcome)
  }

  def getPayout(outcome: Long, rounding: RoundingIntervals): Satoshis = {
    val Indexed(func, _) = componentFor(outcome)
    func(outcome, rounding)
  }

  def apply(outcome: Long): Satoshis = getPayout(outcome)

  def apply(outcome: Long, rounding: RoundingIntervals): Satoshis =
    getPayout(outcome, rounding)
}

/** A point on a DLC payout curve to be used for interpolation
  *
  * outcome: An element of the domain of possible events signed by the oracle
  * payout: The payout to the local party corresponding to outcome
  * isEndpoint: True if this point defines a boundary between pieces in the curve
  */
sealed trait OutcomePayoutPoint {
  def outcome: Long
  def payout: BigDecimal

  def isEndPoint: Boolean = {
    this match {
      case _: OutcomePayoutEndpoint => true
      case _: OutcomePayoutMidpoint => false
    }
  }

  def roundedPayout: Satoshis = {
    Satoshis(payout.setScale(0, RoundingMode.FLOOR).toLongExact)
  }

  def extraPrecision: Int = {
    val shifted = (payout - roundedPayout.toLong) * (1 << 16)
    shifted.setScale(0, RoundingMode.FLOOR).toIntExact
  }

  def copy(
      outcome: Long = this.outcome,
      payout: BigDecimal = this.payout): OutcomePayoutPoint = {
    this match {
      case OutcomePayoutEndpoint(_, _) => OutcomePayoutEndpoint(outcome, payout)
      case OutcomePayoutMidpoint(_, _) => OutcomePayoutMidpoint(outcome, payout)
    }
  }

  /** Converts our internal representation to a TLV that can be sent over the wire */
  def toTlvPoint: TLVPoint = {
    this match {
      case _: OutcomePayoutEndpoint =>
        TLVPoint(outcome = outcome,
                 value = roundedPayout,
                 extraPrecision = extraPrecision,
                 isEndpoint = true)
      case _: OutcomePayoutMidpoint =>
        TLVPoint(outcome = outcome,
                 value = roundedPayout,
                 extraPrecision = extraPrecision,
                 isEndpoint = false)
    }
  }
}

object OutcomePayoutPoint {

  def apply(
      outcome: Long,
      payout: BigDecimal,
      isEndpoint: Boolean): OutcomePayoutPoint = {
    if (isEndpoint) {
      OutcomePayoutEndpoint(outcome, payout)
    } else {
      OutcomePayoutMidpoint(outcome, payout)
    }
  }

  def apply(
      outcome: Long,
      payout: Satoshis,
      isEndpoint: Boolean): OutcomePayoutPoint = {
    OutcomePayoutPoint(outcome, payout.toLong, isEndpoint)
  }
}

case class OutcomePayoutEndpoint(outcome: Long, payout: BigDecimal)
    extends OutcomePayoutPoint {

  def toMidpoint: OutcomePayoutMidpoint = OutcomePayoutMidpoint(outcome, payout)
}

object OutcomePayoutEndpoint {

  def apply(outcome: Long, payout: Satoshis): OutcomePayoutEndpoint = {
    OutcomePayoutEndpoint(outcome, payout.toLong)
  }
}

case class OutcomePayoutMidpoint(outcome: Long, payout: BigDecimal)
    extends OutcomePayoutPoint {

  def toEndpoint: OutcomePayoutEndpoint = OutcomePayoutEndpoint(outcome, payout)
}

object OutcomePayoutMidpoint {

  def apply(outcome: Long, payout: Satoshis): OutcomePayoutMidpoint = {
    OutcomePayoutMidpoint(outcome, payout.toLong)
  }
}

/** A single piece of a larger piecewise function defined between left and right endpoints */
sealed trait DLCPayoutCurveComponent {
  def leftEndpoint: OutcomePayoutEndpoint
  def midpoints: Vector[OutcomePayoutMidpoint]
  def rightEndpoint: OutcomePayoutEndpoint

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

  def apply(outcome: Long): Satoshis

  def apply(outcome: Long, rounding: RoundingIntervals): Satoshis = {
    rounding.round(outcome, apply(outcome))
  }

  /** Returns the largest Long less than or equal to bd (floor function) */
  protected def bigDecimalSats(bd: BigDecimal): Satoshis = {
    Satoshis(
      bd.setScale(6, RoundingMode.HALF_UP)
        .setScale(0, RoundingMode.HALF_UP)
        .toLongExact)
  }
}

object DLCPayoutCurveComponent {

  def apply(points: Vector[OutcomePayoutPoint]): DLCPayoutCurveComponent = {
    require(points.head.isEndPoint && points.last.isEndPoint,
            s"First and last points must be endpoints, $points")
    require(points.tail.init.forall(!_.isEndPoint),
            s"Endpoint detected in middle, $points")

    points match {
      case Vector(left: OutcomePayoutEndpoint, right: OutcomePayoutEndpoint) =>
        if (left.payout == right.payout) {
          OutcomePayoutConstant(left, right)
        } else {
          OutcomePayoutLine(left, right)
        }
      case Vector(left: OutcomePayoutEndpoint,
                  mid: OutcomePayoutMidpoint,
                  right: OutcomePayoutEndpoint) =>
        OutcomePayoutQuadratic(left, mid, right)
      case Vector(left: OutcomePayoutEndpoint,
                  mid1: OutcomePayoutMidpoint,
                  mid2: OutcomePayoutMidpoint,
                  right: OutcomePayoutEndpoint) =>
        OutcomePayoutCubic(left, mid1, mid2, right)
      case _ => OutcomePayoutPolynomial(points)
    }
  }
}

case class OutcomePayoutConstant(
    leftEndpoint: OutcomePayoutEndpoint,
    rightEndpoint: OutcomePayoutEndpoint)
    extends DLCPayoutCurveComponent {
  require(leftEndpoint.payout == rightEndpoint.payout,
          "Constant function must have same values on endpoints")

  override lazy val midpoints: Vector[OutcomePayoutMidpoint] = Vector.empty

  override def apply(outcome: Long): Satoshis =
    bigDecimalSats(leftEndpoint.payout)
}

/** A Line between left and right endpoints defining a piece of a larger payout curve */
case class OutcomePayoutLine(
    leftEndpoint: OutcomePayoutEndpoint,
    rightEndpoint: OutcomePayoutEndpoint)
    extends DLCPayoutCurveComponent {
  override lazy val midpoints: Vector[OutcomePayoutMidpoint] = Vector.empty

  lazy val slope: BigDecimal = {
    (rightEndpoint.payout - leftEndpoint.payout) / (rightEndpoint.outcome - leftEndpoint.outcome)
  }

  override def apply(outcome: Long): Satoshis = {
    val value =
      (outcome - leftEndpoint.outcome) * slope + leftEndpoint.payout

    bigDecimalSats(value)
  }
}

/** A quadratic between left and right endpoints defining a piece of a larger payout curve.
  * A quadratic equation defines a parabola: https://en.wikipedia.org/wiki/Quadratic_function
  */
case class OutcomePayoutQuadratic(
    leftEndpoint: OutcomePayoutEndpoint,
    midpoint: OutcomePayoutMidpoint,
    rightEndpoint: OutcomePayoutEndpoint)
    extends DLCPayoutCurveComponent {
  override lazy val midpoints: Vector[OutcomePayoutMidpoint] = Vector(midpoint)

  private lazy val (x01, x02, x12) =
    (leftEndpoint.outcome - midpoint.outcome,
     leftEndpoint.outcome - rightEndpoint.outcome,
     midpoint.outcome - rightEndpoint.outcome)

  private lazy val (x10, x20, x21) = (-x01, -x02, -x12)

  private lazy val (y0, y1, y2) =
    (leftEndpoint.payout, midpoint.payout, rightEndpoint.payout)

  private lazy val (c0, c1, c2) =
    (y0 / (x01 * x02), y1 / (x10 * x12), y2 / (x20 * x21))

  override def apply(outcome: Long): Satoshis = {
    val x0 = outcome - leftEndpoint.outcome
    val x1 = outcome - midpoint.outcome
    val x2 = outcome - rightEndpoint.outcome

    val value = c0 * (x1 * x2) + c1 * (x0 * x2) + c2 * (x0 * x1)

    bigDecimalSats(value)
  }
}

/** A cubic between left and right endpoints defining a piece of a larger payout curve */
case class OutcomePayoutCubic(
    leftEndpoint: OutcomePayoutEndpoint,
    leftMidpoint: OutcomePayoutMidpoint,
    rightMidpoint: OutcomePayoutMidpoint,
    rightEndpoint: OutcomePayoutEndpoint)
    extends DLCPayoutCurveComponent {

  override lazy val midpoints: Vector[OutcomePayoutMidpoint] =
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

  private lazy val (y0, y1, y2, y3) = (leftEndpoint.payout,
                                       leftMidpoint.payout,
                                       rightMidpoint.payout,
                                       rightEndpoint.payout)

  private lazy val (c0, c1, c2, c3) =
    (y0 / (x01 * x02 * x03),
     y1 / (x10 * x12 * x13),
     y2 / (x20 * x21 * x23),
     y3 / (x30 * x31 * x32))

  override def apply(outcome: Long): Satoshis = {
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
  require(points.head.isInstanceOf[OutcomePayoutEndpoint] && points.last
            .isInstanceOf[OutcomePayoutEndpoint],
          s"First and last points must be endpoints, $points")
  require(points.tail.init.forall(!_.isInstanceOf[OutcomePayoutEndpoint]),
          s"Endpoint detected in middle, $points")

  override lazy val leftEndpoint: OutcomePayoutEndpoint =
    points.head.asInstanceOf[OutcomePayoutEndpoint]

  override lazy val rightEndpoint: OutcomePayoutEndpoint =
    points.last.asInstanceOf[OutcomePayoutEndpoint]

  override lazy val midpoints: Vector[OutcomePayoutMidpoint] =
    points.tail.init.asInstanceOf[Vector[OutcomePayoutMidpoint]]

  lazy val coefficients: Vector[BigDecimal] = {
    points.map { point =>
      val xi = point.outcome
      val yi = point.payout

      val denom = points.foldLeft(BigDecimal(1)) {
        case (prodSoFar, p) =>
          val xj = p.outcome

          if (xj == xi) {
            prodSoFar
          } else {
            prodSoFar * (xi - xj)
          }
      }

      yi / denom
    }
  }

  override def apply(outcome: Long): Satoshis = {
    points.find(_.outcome == outcome) match {
      case Some(point) => bigDecimalSats(point.payout)
      case None =>
        val allProd = points.foldLeft(BigDecimal(1)) {
          case (prodSoFar, point) =>
            prodSoFar * (outcome - point.outcome)
        }

        val value = coefficients.zipWithIndex.foldLeft(BigDecimal(0)) {
          case (sumSoFar, (coefficientI, i)) =>
            sumSoFar + (coefficientI * allProd / (outcome - points(i).outcome))
        }

        bigDecimalSats(value)
    }
  }
}
