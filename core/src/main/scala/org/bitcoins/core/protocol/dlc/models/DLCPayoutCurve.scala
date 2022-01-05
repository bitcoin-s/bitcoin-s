package org.bitcoins.core.protocol.dlc.models

import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.util.{Indexed, NumberUtil}

import scala.math.BigDecimal.RoundingMode
import scala.util.{Failure, Success, Try}

/** A DLC payout curve defined by piecewise interpolating points */
case class DLCPayoutCurve(
    pieces: Vector[DLCPayoutCurvePiece],
    serializationVersion: DLCSerializationVersion)
    extends TLVSerializable[PayoutFunctionV0TLV] {

  val endpoints: Vector[OutcomePayoutPoint] = {
    pieces.map(_.leftEndpoint).:+(pieces.last.rightEndpoint)
  }

  require(pieces.map(_.rightEndpoint) == endpoints.tail,
          s"Endpoints must line up: $this")

  override def toTLV: PayoutFunctionV0TLV = {
    val tlvEndpoints = endpoints.map(_.toTLVPoint)
    val tlvPieces = pieces.map(_.toTLV)

    PayoutFunctionV0TLV(tlvEndpoints, tlvPieces, serializationVersion)
  }

  private lazy val endpointOutcomes = endpoints.map(_.outcome)

  /** Returns the function component on which the given oracle outcome is
    * defined, along with its index
    */
  def componentFor(outcome: Long): Indexed[DLCPayoutCurvePiece] = {
    val endpointIndex = NumberUtil.search(endpointOutcomes, outcome)
    val endpoint = endpoints(endpointIndex)

    if (endpoint.outcome == outcome && endpointIndex != pieces.length) {
      Indexed(pieces(endpointIndex), endpointIndex)
    } else {
      Indexed(pieces(endpointIndex - 1), endpointIndex - 1)
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

  def getPayout(
      outcome: Long,
      rounding: RoundingIntervals,
      totalCollateral: Satoshis): Satoshis = {
    val Indexed(func, _) = componentFor(outcome)
    func(outcome, rounding, totalCollateral)
  }

  def apply(outcome: Long): Satoshis = getPayout(outcome)

  def apply(outcome: Long, rounding: RoundingIntervals): Satoshis =
    getPayout(outcome, rounding)

  def apply(
      outcome: Long,
      rounding: RoundingIntervals,
      totalCollateral: Satoshis): Satoshis =
    getPayout(outcome, rounding, totalCollateral)

  def flip(totalCollateral: Satoshis): DLCPayoutCurve = {
    DLCPayoutCurve(pieces.map(_.flip(totalCollateral)),
                   serializationVersion = serializationVersion)
  }
}

object DLCPayoutCurve
    extends TLVDeserializable[PayoutFunctionV0TLV, DLCPayoutCurve](
      PayoutFunctionV0TLV) {

  override def fromTLV(tlv: PayoutFunctionV0TLV): DLCPayoutCurve = {
    val pieces =
      tlv.endpoints.init.zip(tlv.endpoints.tail).zip(tlv.pieces).map {
        case ((leftEndpoint, rightEndpoint), tlvPiece) =>
          DLCPayoutCurvePiece.fromTLV(leftEndpoint, tlvPiece, rightEndpoint)
      }

    DLCPayoutCurve(pieces, tlv.serializationVersion)
  }

  def polynomialInterpolate(
      points: Vector[PiecewisePolynomialPoint],
      serializationVersion: DLCSerializationVersion): DLCPayoutCurve = {
    require(points.head.isEndpoint && points.last.isEndpoint,
            s"First and last points must be endpoints: $points")

    val initMidpoints = Vector.empty[PiecewisePolynomialMidpoint]
    val initCurvePieces = Vector.empty[DLCPolynomialPayoutCurvePiece]
    val (_, _, pieces) =
      points.tail.foldLeft((points.head, initMidpoints, initCurvePieces)) {
        case ((lastEndpoint, midpointsSoFar, piecesSoFar), point) =>
          point match {
            case midpoint: PiecewisePolynomialMidpoint =>
              (lastEndpoint, midpointsSoFar.:+(midpoint), piecesSoFar)
            case endpoint: PiecewisePolynomialEndpoint =>
              val all = midpointsSoFar
                .+:(lastEndpoint)
                .:+(endpoint)
              val points = all.map(_.toOutcomePayoutPoint)
              (endpoint,
               Vector.empty,
               piecesSoFar.:+(DLCPolynomialPayoutCurvePiece(points)))
          }
      }
    DLCPayoutCurve(pieces, serializationVersion)
  }

  def fromPoints(
      points: Vector[TLVPoint],
      serializationVersion: DLCSerializationVersion): DLCPayoutCurve = {

    val pieceEndpoints = points.map { p =>
      PiecewisePolynomialEndpoint(p.outcome, p.value)
    }

    DLCPayoutCurve.polynomialInterpolate(pieceEndpoints, serializationVersion)
  }

  def fromPointsPre144(points: Vector[OldTLVPoint]): DLCPayoutCurve = {
    val newPoints =
      points.map(p => TLVPoint(p.outcome, p.value, p.extraPrecision))
    fromPoints(newPoints, serializationVersion = DLCSerializationVersion.Alpha)
  }
}

trait DLCPoint {
  def outcome: Long
  def payout: BigDecimal

  def roundedPayout: Satoshis = {
    Satoshis(payout.setScale(0, RoundingMode.FLOOR).toLongExact)
  }

  def extraPrecision: Int = {
    val shifted = (payout - roundedPayout.toLong) * (1 << 16)
    shifted.setScale(0, RoundingMode.FLOOR).toIntExact
  }

  def toTLVPoint: TLVPoint = {
    TLVPoint(outcome, roundedPayout, extraPrecision)
  }

  def toOutcomePayoutPoint: OutcomePayoutPoint = {
    OutcomePayoutPoint(outcome = outcome, payout = payout)
  }
}

/** A point on a DLC payout curve to be used for interpolation
  *
  * outcome: An element of the domain of possible events signed by the oracle
  * payout: The payout to the local party corresponding to outcome
  */
case class OutcomePayoutPoint(outcome: Long, payout: BigDecimal)
    extends DLCPoint {

  override def toString: String = {
    s"OutcomePayoutPoint(outcome=$outcome,payout=$payout)"
  }
}

object OutcomePayoutPoint {

  def apply(outcome: Long, payout: Satoshis): OutcomePayoutPoint = {
    OutcomePayoutPoint(outcome, payout.toLong)
  }

  def fromTLVPoint(point: TLVPoint): OutcomePayoutPoint = {
    OutcomePayoutPoint(point.outcome, point.bigDecimalPayout)
  }
}

sealed trait PiecewisePolynomialPoint extends DLCPoint {

  /** True if this point defines a boundary between pieces in the curve */
  def isEndpoint: Boolean
}

object PiecewisePolynomialPoint {

  def apply(
      outcome: Long,
      payout: BigDecimal,
      isEndpoint: Boolean): PiecewisePolynomialPoint = {
    if (isEndpoint) {
      PiecewisePolynomialEndpoint(outcome, payout)
    } else {
      PiecewisePolynomialMidpoint(outcome, payout)
    }
  }

  def apply(
      outcome: Long,
      payout: CurrencyUnit,
      isEndpoint: Boolean): PiecewisePolynomialPoint = {
    PiecewisePolynomialPoint(outcome, payout.toBigDecimal, isEndpoint)
  }
}

case class PiecewisePolynomialEndpoint(outcome: Long, payout: BigDecimal)
    extends PiecewisePolynomialPoint {
  override def isEndpoint: Boolean = true
}

object PiecewisePolynomialEndpoint {

  def apply(outcome: Long, payout: Satoshis): PiecewisePolynomialEndpoint = {
    PiecewisePolynomialEndpoint(outcome, payout.toBigDecimal)
  }
}

case class PiecewisePolynomialMidpoint(outcome: Long, payout: BigDecimal)
    extends PiecewisePolynomialPoint {
  override def isEndpoint: Boolean = false
}

object PiecewisePolynomialMidpoint {

  def apply(outcome: Long, payout: Satoshis): PiecewisePolynomialMidpoint = {
    PiecewisePolynomialMidpoint(outcome, payout.toBigDecimal)
  }
}

sealed trait DLCPayoutCurvePiece extends TLVSerializable[PayoutCurvePieceTLV] {
  def leftEndpoint: OutcomePayoutPoint
  def rightEndpoint: OutcomePayoutPoint

  require(leftEndpoint.outcome < rightEndpoint.outcome,
          s"Points must be ascending: $this")

  def apply(outcome: Long): Satoshis

  def apply(outcome: Long, rounding: RoundingIntervals): Satoshis = {
    rounding.round(outcome, apply(outcome))
  }

  def apply(
      outcome: Long,
      rounding: RoundingIntervals,
      totalCollateral: Satoshis): Satoshis = {
    val rounded = rounding.round(outcome, apply(outcome)).toLong
    val modified = math.min(math.max(rounded, 0), totalCollateral.toLong)

    Satoshis(modified)
  }

  /** Returns the largest Long less than or equal to bd (floor function) */
  protected def bigDecimalSats(bd: BigDecimal): Satoshis = {
    Satoshis(
      bd.setScale(6, RoundingMode.HALF_UP)
        .setScale(0, RoundingMode.FLOOR)
        .toLongExact)
  }

  def flip(totalCollateral: Satoshis): DLCPayoutCurvePiece
}

object DLCPayoutCurvePiece {

  def fromTLV(
      leftEndpoint: TLVPoint,
      curvePiece: PayoutCurvePieceTLV,
      rightEndpoint: TLVPoint): DLCPayoutCurvePiece = {
    curvePiece match {
      case polynomial: PolynomialPayoutCurvePieceTLV =>
        DLCPolynomialPayoutCurvePiece.fromTLV(leftEndpoint,
                                              polynomial,
                                              rightEndpoint)
      case hyperbola: HyperbolaPayoutCurvePieceTLV =>
        DLCHyperbolaPayoutCurvePiece.fromTLV(leftEndpoint,
                                             hyperbola,
                                             rightEndpoint)
    }
  }
}

case class DLCHyperbolaPayoutCurvePiece(
    usePositivePiece: Boolean,
    translateOutcome: BigDecimal,
    translatePayout: BigDecimal,
    a: BigDecimal,
    b: BigDecimal,
    c: BigDecimal,
    d: BigDecimal,
    leftEndpoint: OutcomePayoutPoint,
    rightEndpoint: OutcomePayoutPoint)
    extends DLCPayoutCurvePiece
    with TLVSerializable[HyperbolaPayoutCurvePieceTLV] {
  require(a * d != b * c, s"a*d cannot equal b*c: $this")

  override def apply(outcome: Long): Satoshis = {
    val resultT = Try {
      val translatedOutcome: BigDecimal = outcome - translateOutcome

      val sqrtTermAbsVal: BigDecimal =
        BigDecimal(math.sqrt((translatedOutcome.pow(2) - 4 * a * b).toDouble))

      val sqrtTerm: BigDecimal =
        if (usePositivePiece) sqrtTermAbsVal else -sqrtTermAbsVal

      val firstTerm = c * (translatedOutcome + sqrtTerm) / (2 * a)
      val secondTerm = 2 * a * d / (translatedOutcome + sqrtTerm)

      val value = firstTerm + secondTerm + translatePayout

      bigDecimalSats(value)
    }

    resultT match {
      case Success(result) => result
      case Failure(err) =>
        throw new IllegalArgumentException(s"Illegal input outcome $outcome.",
                                           err)
    }
  }

  override def toTLV: HyperbolaPayoutCurvePieceTLV = {
    HyperbolaPayoutCurvePieceTLV(
      usePositivePiece,
      Signed16PTLVNumber.fromBigDecimal(translateOutcome),
      Signed16PTLVNumber.fromBigDecimal(translatePayout),
      Signed16PTLVNumber.fromBigDecimal(a),
      Signed16PTLVNumber.fromBigDecimal(b),
      Signed16PTLVNumber.fromBigDecimal(c),
      Signed16PTLVNumber.fromBigDecimal(d)
    )
  }

  override def flip(totalCollateral: Satoshis): DLCHyperbolaPayoutCurvePiece = {
    DLCHyperbolaPayoutCurvePiece(
      usePositivePiece,
      translateOutcome,
      totalCollateral.toLong - translatePayout,
      a,
      b,
      -c,
      -d,
      leftEndpoint.copy(payout = totalCollateral.toLong - leftEndpoint.payout),
      rightEndpoint.copy(payout = totalCollateral.toLong - rightEndpoint.payout)
    )
  }
}

object DLCHyperbolaPayoutCurvePiece {

  def fromTLV(
      leftEndpoint: TLVPoint,
      curvePiece: HyperbolaPayoutCurvePieceTLV,
      rightEndpoint: TLVPoint): DLCHyperbolaPayoutCurvePiece = {
    DLCHyperbolaPayoutCurvePiece(
      curvePiece.usePositivePiece,
      curvePiece.translateOutcome.toBigDecimal,
      curvePiece.translatePayout.toBigDecimal,
      curvePiece.a.toBigDecimal,
      curvePiece.b.toBigDecimal,
      curvePiece.c.toBigDecimal,
      curvePiece.d.toBigDecimal,
      OutcomePayoutPoint.fromTLVPoint(leftEndpoint),
      OutcomePayoutPoint.fromTLVPoint(rightEndpoint)
    )
  }
}

/** A single piece of a larger piecewise function defined between left and right endpoints */
sealed trait DLCPolynomialPayoutCurvePiece
    extends DLCPayoutCurvePiece
    with TLVSerializable[PolynomialPayoutCurvePieceTLV] {
  def midpoints: Vector[OutcomePayoutPoint]

  def points: Vector[OutcomePayoutPoint] = {
    midpoints.+:(leftEndpoint).:+(rightEndpoint)
  }

  midpoints.headOption.foreach { firstMidpoint =>
    require(leftEndpoint.outcome < firstMidpoint.outcome,
            s"Points must be ascending: $this")
    require(midpoints.init.zip(midpoints.tail).forall { case (m1, m2) =>
              m1.outcome < m2.outcome
            },
            s"Points must be ascending: $this")
    require(rightEndpoint.outcome > midpoints.last.outcome,
            s"Points must be ascending: $this")
  }

  override def toTLV: PolynomialPayoutCurvePieceTLV = {
    PolynomialPayoutCurvePieceTLV(midpoints.map(_.toTLVPoint))
  }

  override def flip(
      totalCollateral: Satoshis): DLCPolynomialPayoutCurvePiece = {
    val flippedPoints = points.map { point =>
      point.copy(payout = totalCollateral.toLong - point.payout)
    }

    DLCPolynomialPayoutCurvePiece(flippedPoints)
  }
}

object DLCPolynomialPayoutCurvePiece {

  def apply(
      points: Vector[OutcomePayoutPoint]): DLCPolynomialPayoutCurvePiece = {
    points match {
      case Vector(left: OutcomePayoutPoint, right: OutcomePayoutPoint) =>
        if (left.payout == right.payout) {
          OutcomePayoutConstant(left, right)
        } else {
          OutcomePayoutLine(left, right)
        }
      case Vector(left: OutcomePayoutPoint,
                  mid: OutcomePayoutPoint,
                  right: OutcomePayoutPoint) =>
        OutcomePayoutQuadratic(left, mid, right)
      case Vector(left: OutcomePayoutPoint,
                  mid1: OutcomePayoutPoint,
                  mid2: OutcomePayoutPoint,
                  right: OutcomePayoutPoint) =>
        OutcomePayoutCubic(left, mid1, mid2, right)
      case _ => OutcomePayoutPolynomial(points)
    }
  }

  def fromTLV(
      leftEndpoint: TLVPoint,
      curvePiece: PolynomialPayoutCurvePieceTLV,
      rightEndpoint: TLVPoint): DLCPolynomialPayoutCurvePiece = {
    val tlvPoints = curvePiece.midpoints.+:(leftEndpoint).:+(rightEndpoint)
    val points = tlvPoints.map(OutcomePayoutPoint.fromTLVPoint)

    DLCPolynomialPayoutCurvePiece(points)
  }
}

case class OutcomePayoutConstant(
    leftEndpoint: OutcomePayoutPoint,
    rightEndpoint: OutcomePayoutPoint)
    extends DLCPolynomialPayoutCurvePiece {
  require(leftEndpoint.payout == rightEndpoint.payout,
          "Constant function must have same values on endpoints")

  override lazy val midpoints: Vector[OutcomePayoutPoint] = Vector.empty

  override def apply(outcome: Long): Satoshis =
    bigDecimalSats(leftEndpoint.payout)
}

/** A Line between left and right endpoints defining a piece of a larger payout curve */
case class OutcomePayoutLine(
    leftEndpoint: OutcomePayoutPoint,
    rightEndpoint: OutcomePayoutPoint)
    extends DLCPolynomialPayoutCurvePiece {
  override lazy val midpoints: Vector[OutcomePayoutPoint] = Vector.empty

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
    leftEndpoint: OutcomePayoutPoint,
    midpoint: OutcomePayoutPoint,
    rightEndpoint: OutcomePayoutPoint)
    extends DLCPolynomialPayoutCurvePiece {
  override lazy val midpoints: Vector[OutcomePayoutPoint] = Vector(midpoint)

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
    leftEndpoint: OutcomePayoutPoint,
    leftMidpoint: OutcomePayoutPoint,
    rightMidpoint: OutcomePayoutPoint,
    rightEndpoint: OutcomePayoutPoint)
    extends DLCPolynomialPayoutCurvePiece {

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
case class OutcomePayoutPolynomial(
    override val points: Vector[OutcomePayoutPoint])
    extends DLCPolynomialPayoutCurvePiece {

  override lazy val leftEndpoint: OutcomePayoutPoint =
    points.head

  override lazy val rightEndpoint: OutcomePayoutPoint =
    points.last

  override lazy val midpoints: Vector[OutcomePayoutPoint] =
    points.tail.init

  lazy val coefficients: Vector[BigDecimal] = {
    points.map { point =>
      val xi = point.outcome
      val yi = point.payout

      val denom = points.foldLeft(BigDecimal(1)) { case (prodSoFar, p) =>
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
